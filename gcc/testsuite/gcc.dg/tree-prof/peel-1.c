/* { dg-options "-O3 -fdump-rtl-loop2_unroll -fno-unroll-loops -fpeel-loops" } */
void abort();

int a[1000];
int
__attribute__ ((noinline))
t()
{
  int i;
  for (i=0;i<1000;i++)
    if (!a[i])
      return 1;
  abort ();
}
main()
{
  int i;
  for (i=0;i<1000;i++)
    t();
  return 0;
}
/* { dg-final-use { scan-rtl-dump "Considering simply peeling loop" "loop2_unroll" } } */
/* In fact one peeling is enough; we however mispredict number of iterations of the loop
   at least until loop_ch is schedule ahead of profiling pass.  */
/* { dg-final-use { cleanup-rtl-dump "loop2_unroll" } } */
