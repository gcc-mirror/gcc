/* { dg-require-alias "" } */
/* { dg-skip-if "" { powerpc-ibm-aix* } } */
static int a=0;
extern int b __attribute__ ((alias("a")));
__attribute__ ((noinline))
static inc()
{
  b++;
}
int
main()
{
  a=0;
  inc ();
  if (a!=1)
    __builtin_abort ();
  return 0;
}
