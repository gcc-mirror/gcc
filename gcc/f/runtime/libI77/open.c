#ifndef NON_UNIX_STDIO
#include <sys/types.h>
#include <sys/stat.h>
#endif
#include "f2c.h"
#include "fio.h"
#include <string.h>
#include "rawio.h"

#ifdef KR_headers
extern char *malloc(), *mktemp();
extern integer f_clos();
#else
#undef abs
#undef min
#undef max
#include <stdlib.h>
extern int f__canseek(FILE*);
extern integer f_clos(cllist*);
#endif

#ifdef NON_ANSI_RW_MODES
char *f__r_mode[2] = {"r", "r"};
char *f__w_mode[4] = {"w", "w", "r+w", "r+w"};
#else
char *f__r_mode[2] = {"rb", "r"};
char *f__w_mode[4] = {"wb", "w", "r+b", "r+"};
#endif

#ifdef KR_headers
f__isdev(s) char *s;
#else
f__isdev(char *s)
#endif
{
#ifdef NON_UNIX_STDIO
	int i, j;

	i = open(s,O_RDONLY);
	if (i == -1)
		return 0;
	j = isatty(i);
	close(i);
	return j;
#else
	struct stat x;

	if(stat(s, &x) == -1) return(0);
#ifdef S_IFMT
	switch(x.st_mode&S_IFMT) {
		case S_IFREG:
		case S_IFDIR:
			return(0);
		}
#else
#ifdef S_ISREG
	/* POSIX version */
	if(S_ISREG(x.st_mode) || S_ISDIR(x.st_mode))
		return(0);
	else
#else
	Help! How does stat work on this system?
#endif
#endif
		return(1);
#endif
}
#ifdef KR_headers
integer f_open(a) olist *a;
#else
integer f_open(olist *a)
#endif
{	unit *b;
	integer rv;
	char buf[256], *s;
	cllist x;
	int ufmt;
#ifdef NON_UNIX_STDIO
	FILE *tf;
#else
	int n;
	struct stat stb;
#endif
	if(f__init != 1) f_init();
	if(a->ounit>=MXUNIT || a->ounit<0)
		err(a->oerr,101,"open");
	f__curunit = b = &f__units[a->ounit];
	if(b->ufd) {
		if(a->ofnm==0)
		{
		same:	if (a->oblnk)
				b->ublnk = *a->oblnk == 'z' || *a->oblnk == 'Z';
			return(0);
		}
#ifdef NON_UNIX_STDIO
		if (b->ufnm
		 && strlen(b->ufnm) == a->ofnmlen
		 && !strncmp(b->ufnm, b->ufnm, (unsigned)a->ofnmlen))
			goto same;
#else
		g_char(a->ofnm,a->ofnmlen,buf);
		if (f__inode(buf,&n) == b->uinode && n == b->udev)
			goto same;
#endif
		x.cunit=a->ounit;
		x.csta=0;
		x.cerr=a->oerr;
		if ((rv = f_clos(&x)) != 0)
			return rv;
		}
	b->url = (int)a->orl;
	b->ublnk = a->oblnk && (*a->oblnk == 'z' || *a->oblnk == 'Z');
	if(a->ofm==0)
	{	if(b->url>0) b->ufmt=0;
		else b->ufmt=1;
	}
	else if(*a->ofm=='f' || *a->ofm == 'F') b->ufmt=1;
	else b->ufmt=0;
	ufmt = b->ufmt;
#ifdef url_Adjust
	if (b->url && !ufmt)
		url_Adjust(b->url);
#endif
	if (a->ofnm) {
		g_char(a->ofnm,a->ofnmlen,buf);
		if (!buf[0])
			err(a->oerr,107,"open");
		}
	else
		sprintf(buf, "fort.%ld", a->ounit);
	b->uscrtch = 0;
	switch(a->osta ? *a->osta : 'u')
	{
	case 'o':
	case 'O':
#ifdef NON_UNIX_STDIO
		if(access(buf,0))
#else
		if(stat(buf,&stb))
#endif
			err(a->oerr,errno,"open");
		break;
	 case 's':
	 case 'S':
		b->uscrtch=1;
#ifdef _POSIX_SOURCE
		tmpnam(buf);
#else
		(void) strcpy(buf,"tmp.FXXXXXX");
		(void) mktemp(buf);
#endif
		goto replace;
	case 'n':
	case 'N':
#ifdef NON_UNIX_STDIO
		if(!access(buf,0))
#else
		if(!stat(buf,&stb))
#endif
			err(a->oerr,128,"open");
		/* no break */
	case 'r':	/* Fortran 90 replace option */
	case 'R':
 replace:
#ifdef NON_UNIX_STDIO
		if (tf = fopen(buf,f__w_mode[0]))
			fclose(tf);
#else
		(void) close(creat(buf, 0666));
#endif
	}

	b->ufnm=(char *) malloc((unsigned int)(strlen(buf)+1));
	if(b->ufnm==NULL) err(a->oerr,113,"no space");
	(void) strcpy(b->ufnm,buf);
	b->uend=0;
	b->uwrt = 0;
#ifdef NON_UNIX_STDIO
	if ((s = a->oacc) && (*s == 'd' || *s == 'D'))
		ufmt = 0;
#endif
	if(f__isdev(buf))
	{	b->ufd = fopen(buf,f__r_mode[ufmt]);
		if(b->ufd==NULL) err(a->oerr,errno,buf);
	}
	else {
		if(!(b->ufd = fopen(buf, f__r_mode[ufmt]))) {
#ifdef NON_UNIX_STDIO
			if (b->ufd = fopen(buf, f__w_mode[ufmt|2]))
				b->uwrt = 2;
			else if (b->ufd = fopen(buf, f__w_mode[ufmt]))
				b->uwrt = 1;
			else
#else
			if ((n = open(buf,O_WRONLY)) >= 0)
				b->uwrt = 2;
			else {
				n = creat(buf, 0666);
				b->uwrt = 1;
				}
			if (n < 0
			|| (b->ufd = fdopen(n, f__w_mode[ufmt])) == NULL)
#endif
				err(a->oerr, errno, "open");
			}
	}
	b->useek=f__canseek(b->ufd);
#ifndef NON_UNIX_STDIO
	if((b->uinode=f__inode(buf,&b->udev))==-1)
		err(a->oerr,108,"open");
#endif
	if(b->useek)
		if (a->orl)
			rewind(b->ufd);
		else if ((s = a->oacc) && (*s == 'a' || *s == 'A')
			&& fseek(b->ufd, 0L, SEEK_END))
				err(a->oerr,129,"open");
	return(0);
}
#ifdef KR_headers
fk_open(seq,fmt,n) ftnint n;
#else
fk_open(int seq, int fmt, ftnint n)
#endif
{	char nbuf[10];
	olist a;
	int rtn;
	int save_init;

	(void) sprintf(nbuf,"fort.%ld",n);
	a.oerr=1;
	a.ounit=n;
	a.ofnm=nbuf;
	a.ofnmlen=strlen(nbuf);
	a.osta=NULL;
	a.oacc= seq==SEQ?"s":"d";
	a.ofm = fmt==FMT?"f":"u";
	a.orl = seq==DIR?1:0;
	a.oblnk=NULL;
	save_init = f__init;
	f__init &= ~2;
	rtn = f_open(&a);
	f__init = save_init | 1;
	return rtn;
}
