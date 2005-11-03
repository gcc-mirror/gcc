/* { dg-options "-Os -mlong-double-128" } */
/* { dg-do compile { target rs6000-*-* powerpc-*-* } } */
/* Make sure compiler doesn't generate [reg+reg] address mode
   for long doubles. */
union arg {
  int intarg;
  long double longdoublearg;
};
long double d;
int va(int n, union arg **argtable)
{
  (*argtable)[n].longdoublearg = d;
}
