#include "f2c.h"
#include "fio.h"
#include "fmt.h"
#include "lio.h"

ftnint L_len;
int f__Aquote;

 static VOID
donewrec(Void)
{
	if (f__recpos)
		(*f__donewrec)();
	}

#ifdef KR_headers
t_putc(c)
#else
t_putc(int c)
#endif
{
	f__recpos++;
	putc(c,f__cf);
	return(0);
}
 static VOID
#ifdef KR_headers
lwrt_I(n) longint n;
#else
lwrt_I(longint n)
#endif
{
	char *p;
	int ndigit, sign;

	p = f__icvt(n, &ndigit, &sign, 10);
	if(f__recpos + ndigit >= L_len)
		donewrec();
	PUT(' ');
	if (sign)
		PUT('-');
	while(*p)
		PUT(*p++);
}
 static VOID
#ifdef KR_headers
lwrt_L(n, len) ftnint n; ftnlen len;
#else
lwrt_L(ftnint n, ftnlen len)
#endif
{
	if(f__recpos+LLOGW>=L_len)
		donewrec();
	wrt_L((Uint *)&n,LLOGW, len);
}
 static VOID
#ifdef KR_headers
lwrt_A(p,len) char *p; ftnlen len;
#else
lwrt_A(char *p, ftnlen len)
#endif
{
	int a;
	char *p1, *pe;

	a = 0;
	pe = p + len;
	if (f__Aquote) {
		a = 3;
		if (len > 1 && p[len-1] == ' ') {
			while(--len > 1 && p[len-1] == ' ');
			pe = p + len;
			}
		p1 = p;
		while(p1 < pe)
			if (*p1++ == '\'')
				a++;
		}
	if(f__recpos+len+a >= L_len)
		donewrec();
	if (a
#ifndef OMIT_BLANK_CC
		|| !f__recpos
#endif
		)
		PUT(' ');
	if (a) {
		PUT('\'');
		while(p < pe) {
			if (*p == '\'')
				PUT('\'');
			PUT(*p++);
			}
		PUT('\'');
		}
	else
		while(p < pe)
			PUT(*p++);
}

 static int
#ifdef KR_headers
l_g(buf, n) char *buf; double n;
#else
l_g(char *buf, double n)
#endif
{
#ifdef Old_list_output
	doublereal absn;
	char *fmt;

	absn = n;
	if (absn < 0)
		absn = -absn;
	fmt = LLOW <= absn && absn < LHIGH ? LFFMT : LEFMT;
#ifdef USE_STRLEN
	sprintf(buf, fmt, n);
	return strlen(buf);
#else
	return sprintf(buf, fmt, n);
#endif

#else
	register char *b, c, c1;

	b = buf;
	*b++ = ' ';
	if (n < 0) {
		*b++ = '-';
		n = -n;
		}
	else
		*b++ = ' ';
	if (n == 0) {
		*b++ = '0';
		*b++ = '.';
		*b = 0;
		goto f__ret;
		}
	sprintf(b, LGFMT, n);
	switch(*b) {
#ifndef WANT_LEAD_0
		case '0':
			while(b[0] = b[1])
				b++;
			break;
#endif
		case 'i':
		case 'I':
			/* Infinity */
		case 'n':
		case 'N':
			/* NaN */
			while(*++b);
			break;

		default:
	/* Fortran 77 insists on having a decimal point... */
		    for(;; b++)
			switch(*b) {
			case 0:
				*b++ = '.';
				*b = 0;
				goto f__ret;
			case '.':
				while(*++b);
				goto f__ret;
			case 'E':
				for(c1 = '.', c = 'E';  *b = c1;
					c1 = c, c = *++b);
				goto f__ret;
			}
		}
 f__ret:
	return b - buf;
#endif
	}

 static VOID
#ifdef KR_headers
l_put(s) register char *s;
#else
l_put(register char *s)
#endif
{
#ifdef KR_headers
	register int c, (*pn)() = f__putn;
#else
	register int c, (*pn)(int) = f__putn;
#endif
	while(c = *s++)
		(*pn)(c);
	}

 static VOID
#ifdef KR_headers
lwrt_F(n) double n;
#else
lwrt_F(double n)
#endif
{
	char buf[LEFBL];

	if(f__recpos + l_g(buf,n) >= L_len)
		donewrec();
	l_put(buf);
}
 static VOID
#ifdef KR_headers
lwrt_C(a,b) double a,b;
#else
lwrt_C(double a, double b)
#endif
{
	char *ba, *bb, bufa[LEFBL], bufb[LEFBL];
	int al, bl;

	al = l_g(bufa, a);
	for(ba = bufa; *ba == ' '; ba++)
		--al;
	bl = l_g(bufb, b) + 1;	/* intentionally high by 1 */
	for(bb = bufb; *bb == ' '; bb++)
		--bl;
	if(f__recpos + al + bl + 3 >= L_len)
		donewrec();
#ifdef OMIT_BLANK_CC
	else
#endif
	PUT(' ');
	PUT('(');
	l_put(ba);
	PUT(',');
	if (f__recpos + bl >= L_len) {
		(*f__donewrec)();
#ifndef OMIT_BLANK_CC
		PUT(' ');
#endif
		}
	l_put(bb);
	PUT(')');
}
#ifdef KR_headers
l_write(number,ptr,len,type) ftnint *number,type; char *ptr; ftnlen len;
#else
l_write(ftnint *number, char *ptr, ftnlen len, ftnint type)
#endif
{
#define Ptr ((flex *)ptr)
	int i;
	longint x;
	double y,z;
	real *xx;
	doublereal *yy;
	for(i=0;i< *number; i++)
	{
		switch((int)type)
		{
		default: f__fatal(204,"unknown type in lio");
		case TYINT1:
			x = Ptr->flchar;
			goto xint;
		case TYSHORT:
			x=Ptr->flshort;
			goto xint;
#ifdef Allow_TYQUAD
		case TYQUAD:
			x = Ptr->fllongint;
			goto xint;
#endif
		case TYLONG:
			x=Ptr->flint;
		xint:	lwrt_I(x);
			break;
		case TYREAL:
			y=Ptr->flreal;
			goto xfloat;
		case TYDREAL:
			y=Ptr->fldouble;
		xfloat: lwrt_F(y);
			break;
		case TYCOMPLEX:
			xx= &Ptr->flreal;
			y = *xx++;
			z = *xx;
			goto xcomplex;
		case TYDCOMPLEX:
			yy = &Ptr->fldouble;
			y= *yy++;
			z = *yy;
		xcomplex:
			lwrt_C(y,z);
			break;
		case TYLOGICAL1:
			x = Ptr->flchar;
			goto xlog;
		case TYLOGICAL2:
			x = Ptr->flshort;
			goto xlog;
		case TYLOGICAL:
			x = Ptr->flint;
		xlog:	lwrt_L(Ptr->flint, len);
			break;
		case TYCHAR:
			lwrt_A(ptr,len);
			break;
		}
		ptr += len;
	}
	return(0);
}
