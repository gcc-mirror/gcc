/* Test whether a symbol ends up to be accessed directly.  */
/* { dg-do compile { target fpic } } */
/* { dg-options "-O2 -fpic -w" } */
/* { dg-final { scan-assembler-times "@_nl_C_locobj" 1 } } */
/* { dg-final { scan-assembler-times "@_nl_C_locobj@gotpc" 1 } } */
struct {
  int a;
} _nl_C_locobj;

int b;
c()
{
  char *d;
  for (; d[0];)
    if (b)
      if (c < '9')
	{
	  char e = ({ (&_nl_C_locobj)->a; });
	  if (e == 'i' && f())
	    f(0, 0, 0, &_nl_C_locobj);
	  e == 'n' && f(0, 0, 0, _nl_C_locobj);
	}
}
