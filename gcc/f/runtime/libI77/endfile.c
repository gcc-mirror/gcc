#include "f2c.h"
#include "fio.h"
#include <sys/types.h>
#include "rawio.h"

#ifdef KR_headers
extern char *strcpy();
#else
#undef abs
#undef min
#undef max
#include <stdlib.h>
#include <string.h>
#endif

#ifdef NON_UNIX_STDIO
#ifndef unlink
#define unlink remove
#endif
#else
#if defined (MSDOS) && !defined (GO32)
#include "io.h"
#endif
#endif

#ifdef NON_UNIX_STDIO
extern char *f__r_mode[], *f__w_mode[];
#endif

#ifdef KR_headers
integer f_end(a) alist *a;
#else
integer f_end(alist *a)
#endif
{
	unit *b;
	if (f__init & 2)
		f__fatal (131, "I/O recursion");
	if(a->aunit>=MXUNIT || a->aunit<0) err(a->aerr,101,"endfile");
	b = &f__units[a->aunit];
	if(b->ufd==NULL) {
		char nbuf[10];
		(void) sprintf(nbuf,"fort.%ld",a->aunit);
#ifdef NON_UNIX_STDIO
		{ FILE *tf;
			if (tf = fopen(nbuf, f__w_mode[0]))
				fclose(tf);
			}
#else
		close(creat(nbuf, 0666));
#endif
		return(0);
		}
	b->uend=1;
	return(b->useek ? t_runc(a) : 0);
}

 static int
#ifdef NON_UNIX_STDIO
#ifdef KR_headers
copy(from, len, to) char *from, *to; register long len;
#else
copy(FILE *from, register long len, FILE *to)
#endif
{
	int k, len1;
	char buf[BUFSIZ];

	while(fread(buf, len1 = len > BUFSIZ ? BUFSIZ : (int)len, 1, from)) {
		if (!fwrite(buf, len1, 1, to))
			return 1;
		if ((len -= len1) <= 0)
			break;
		}
	return 0;
	}
#else
#ifdef KR_headers
copy(from, len, to) char *from, *to; register long len;
#else
copy(char *from, register long len, char *to)
#endif
{
	register size_t n;
	int k, rc = 0, tmp;
	char buf[BUFSIZ];

	if ((k = open(from, O_RDONLY)) < 0)
		return 1;
	if ((tmp = creat(to,0666)) < 0)
		return 1;
	while((n = read(k, buf, (size_t) (len > BUFSIZ ? BUFSIZ : (int)len))) > 0) {
		if (write(tmp, buf, n) != n)
			{ rc = 1; break; }
		if ((len -= n) <= 0)
			break;
		}
	close(k);
	close(tmp);
	return n < 0 ? 1 : rc;
	}
#endif

#ifndef L_tmpnam
#define L_tmpnam 16
#endif

 int
#ifdef KR_headers
t_runc(a) alist *a;
#else
t_runc(alist *a)
#endif
{
	char nm[L_tmpnam+12];	/* extra space in case L_tmpnam is tiny */
	long loc, len;
	unit *b;
#ifdef NON_UNIX_STDIO
	FILE *bf, *tf;
#else
	FILE *bf;
#endif
	int rc = 0;

	b = &f__units[a->aunit];
	if(b->url)
		return(0);	/*don't truncate direct files*/
	loc=ftell(bf = b->ufd);
	fseek(bf,0L,SEEK_END);
	len=ftell(bf);
	if (loc >= len || b->useek == 0 || b->ufnm == NULL)
		return(0);
#ifdef NON_UNIX_STDIO
	fclose(b->ufd);
#else
	rewind(b->ufd);	/* empty buffer */
#endif
	if (!loc) {
#ifdef NON_UNIX_STDIO
		if (!(bf = fopen(b->ufnm, f__w_mode[b->ufmt])))
#else
		if (close(creat(b->ufnm,0666)))
#endif
			rc = 1;
		if (b->uwrt)
			b->uwrt = 1;
		goto done;
		}
#ifdef _POSIX_SOURCE
	tmpnam(nm);
#else
	strcpy(nm,"tmp.FXXXXXX");
	mktemp(nm);
#endif
#ifdef NON_UNIX_STDIO
	if (!(bf = fopen(b->ufnm, f__r_mode[0]))) {
 bad:
		rc = 1;
		goto done;
		}
	if (!(tf = fopen(nm, f__w_mode[0])))
		goto bad;
	if (copy(bf, loc, tf)) {
 bad1:
		rc = 1;
		goto done1;
		}
	if (!(bf = freopen(b->ufnm, f__w_mode[0], bf)))
		goto bad1;
	if (!(tf = freopen(nm, f__r_mode[0], tf)))
		goto bad1;
	if (copy(tf, loc, bf))
		goto bad1;
	if (f__w_mode[0] != f__w_mode[b->ufmt]) {
	 	if (!(bf = freopen(b->ufnm, f__w_mode[b->ufmt|2], bf)))
			goto bad1;
		fseek(bf, loc, SEEK_SET);
		}
done1:
	fclose(tf);
	unlink(nm);
done:
	f__cf = b->ufd = bf;
#else
	if (copy(b->ufnm, loc, nm)
	 || copy(nm, loc, b->ufnm))
		rc = 1;
	unlink(nm);
	fseek(b->ufd, loc, SEEK_SET);
done:
#endif
	if (rc)
		err(a->aerr,111,"endfile");
	return 0;
	}
