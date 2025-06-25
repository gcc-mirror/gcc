/* { dg-do compile } */
/* { dg-options "-O2 -mstack-arg-probe" } */

short __mingw_swformat_format;
__builtin_va_list __mingw_swformat_arg;
int __mingw_swformat_fc;
typedef struct {
  void *fp;
  int bch[1024];
} _IFP;
void __mingw_swformat(_IFP *s) {
  if (s->fp)
    while (__mingw_swformat_format)
      if (__mingw_swformat_fc == 'A')
	*__builtin_va_arg(__mingw_swformat_arg, double *) = 0;
}
void
__mingw_vswscanf (void)
{
  _IFP ifp;
  __mingw_swformat(&ifp);
}
