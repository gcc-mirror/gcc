#include "f2c.h"
#include "fio.h"
#include "lio.h"
#include "fmt.h" /* for f__doend */

extern flag f__lquit;
extern int f__lcount;
extern char *f__icptr;
extern char *f__icend;
extern icilist *f__svic;
extern int f__icnum, f__recpos;

static int i_getc(Void)
{
	if(f__recpos >= f__svic->icirlen) {
		if (f__recpos++ == f__svic->icirlen)
			return '\n';
		z_rnew();
		}
	f__recpos++;
	if(f__icptr >= f__icend)
		return EOF;
	return(*f__icptr++);
	}

 static
#ifdef KR_headers
int i_ungetc(ch, f) int ch; FILE *f;
#else
int i_ungetc(int ch, FILE *f)
#endif
{
	if (--f__recpos == f__svic->icirlen)
		return '\n';
	if (f__recpos < -1)
		err(f__svic->icierr,110,"recend");
	/* *--icptr == ch, and icptr may point to read-only memory */
	return *--f__icptr /* = ch */;
	}

 static void
#ifdef KR_headers
c_lir(a) icilist *a;
#else
c_lir(icilist *a)
#endif
{
	extern int l_eof;
	if(f__init != 1) f_init();
	f__init = 3;
	f__reading = 1;
	f__external = 0;
	f__formatted = 1;
	f__svic = a;
	L_len = a->icirlen;
	f__recpos = -1;
	f__icnum = f__recpos = 0;
	f__cursor = 0;
	l_getc = i_getc;
	l_ungetc = i_ungetc;
	l_eof = 0;
	f__icptr = a->iciunit;
	f__icend = f__icptr + a->icirlen*a->icirnum;
	f__cf = 0;
	f__curunit = 0;
	f__elist = (cilist *)a;
	}


#ifdef KR_headers
integer s_rsli(a) icilist *a;
#else
integer s_rsli(icilist *a)
#endif
{
	f__lioproc = l_read;
	f__lquit = 0;
	f__lcount = 0;
	c_lir(a);
	f__doend = 0;
	return(0);
	}

integer e_rsli(Void)
{ f__init = 1; return 0; }

#ifdef KR_headers
integer s_rsni(a) icilist *a;
#else
extern int x_rsne(cilist*);

integer s_rsni(icilist *a)
#endif
{
	extern int nml_read;
	integer rv;
	cilist ca;
	ca.ciend = a->iciend;
	ca.cierr = a->icierr;
	ca.cifmt = a->icifmt;
	c_lir(a);
	rv = x_rsne(&ca);
	nml_read = 0;
	return rv;
	}
