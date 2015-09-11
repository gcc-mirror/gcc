/* Test variable number of arguments passed to functions.  */

#include <stdarg.h>
#include "defines.h"


#define ARG_INT     1
#define ARG_DOUBLE  2
#define ARG_POINTER 3

union types
{
  int ivalue;
  double dvalue;
  void *pvalue;
};

struct arg
{
  int type;
  union types value;
};

struct arg *arglist;

/* This tests the argumentlist to see if it matches the format string which
   is printf-like. Nothing will be printed of course. It can handle ints,
   doubles and void pointers. The given value will be tested against the
   values given in arglist.
   This test only assures that the variable argument passing is working.
   No attempt is made to see if argument passing is done the right way.  */
void
__attribute__ ((noinline))
noprintf (char *format, ...)
{
  va_list va_arglist;
  char *c;

  int ivalue;
  double dvalue;
  void *pvalue;
  struct arg *argp = arglist;

  va_start (va_arglist, format);
  for (c = format; *c; c++)
    if (*c == '%')
      {
	switch (*++c)
	  {
	  case 'd':
	    assert (argp->type == ARG_INT);
	    ivalue = va_arg (va_arglist, int);
	    assert (argp->value.ivalue == ivalue);
	    break;
	  case 'f':
	    assert (argp->type == ARG_DOUBLE);
	    dvalue = va_arg (va_arglist, double);
	    assert (argp->value.dvalue == dvalue);
	    break;
	  case 'p':
	    assert (argp->type == ARG_POINTER);
	    pvalue = va_arg (va_arglist, void *);
	    assert (argp->value.pvalue == pvalue);
	    break;
	  default:
	    abort ();
	  }

	argp++;
      }
}

extern void iamcu_noprintf (char *, ...);

int
main (void)
{
#ifdef CHECK_VARARGS
  float f = 258.0;
  struct arg al[5];

  al[0].type = ARG_INT;
  al[0].value.ivalue = 256;
  al[1].type = ARG_DOUBLE;
  al[1].value.dvalue = 257.0;
  al[2].type = ARG_POINTER;
  al[2].value.pvalue = al;
  al[3].type = ARG_DOUBLE;
  al[3].value.dvalue = f;
  al[4].type = ARG_INT;
  al[4].value.ivalue = 259;

  arglist = al;
  noprintf("%d%f%p%f%d", 256, 257.0, al, f, 259);

  iamcu_noprintf ((char *) 0xabadbeef, 256, 257.0,
		  (void *) 0xbbadbeef, f, 259);
#endif

  return 0;
}
