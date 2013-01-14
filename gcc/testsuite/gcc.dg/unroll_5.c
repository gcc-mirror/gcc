/* { dg-do compile } */
/* { dg-options "-O3 -fdump-rtl-loop2_unroll -funroll-loops" } */
/* { dg-require-effective-target int32plus } */

void abort (void);
int *a;
/* Fails on MIPS16 because equality checks are implemented using XOR.
   It's unlikely MIPS16 users would want unrolling anyway.  */
#ifdef __mips
__attribute__((nomips16))
#endif
int t()
{
   int i;
  for (i=0;i<1000000;i++)
    if (a[i])
      return 1;
  return 0;
}
#ifdef __mips
__attribute__((nomips16))
#endif
int t2()
{
   int i;
  for (i=0;i<3000000;i++)
    if (a[i])
        abort ();
  return 0;
}
/* { dg-final { scan-rtl-dump-times "upper bound: 999999" 1 "loop2_unroll" } } */
/* { dg-final { scan-rtl-dump-not "realistic bound: 999999" "loop2_unroll" } } */
/* { dg-final { scan-rtl-dump-times "upper bound: 2999999" 1 "loop2_unroll" } } */
/* { dg-final { scan-rtl-dump-times "realistic bound: 2999999" 1 "loop2_unroll" } } */
/* { dg-final { cleanup-rtl-dump "loop2_unroll" } } */
