/* { dg-require-alias "" } */
/* { dg-skip-if "BSS alias" { powerpc-ibm-aix* } } */
int a[10]={};
extern int b[10] __attribute__ ((alias("a")));
int off;
int
main(void)
{
  b[off]=1;
  a[off]=2;
  if (b[off]!=2)
   __builtin_abort ();
  return 0;
}
