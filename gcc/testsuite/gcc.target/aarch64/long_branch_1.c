/* { dg-do assemble } */
/* { dg-timeout-factor 2.0 } */
/* { dg-options "-O1 -fno-reorder-blocks -fno-tree-cselim -fdisable-tree-ifcombine --save-temps" } */


__attribute__((noinline, noclone)) int
restore (int a, int b)
{
  return a * b;
}

__attribute__((noinline, noclone)) void
do_nothing (int *input)
{
  *input = restore (*input, 1);
  return;
}
#define ENTRY_SUM(n, x) \
    sum = sum / ((n) + (x)); \
    sum = restore (sum, (n) + (x));

#define ENTRY_SUM2(n, x) ENTRY_SUM ((n), (x)) ENTRY_SUM ((n), (x)+1)
#define ENTRY_SUM4(n, x) ENTRY_SUM2 ((n), (x)) ENTRY_SUM2 ((n), (x)+2)
#define ENTRY_SUM8(n, x) ENTRY_SUM4 ((n), (x)) ENTRY_SUM4 ((n), (x)+4)
#define ENTRY_SUM16(n, x) ENTRY_SUM8 ((n), (x)) ENTRY_SUM8 ((n), (x)+8)
#define ENTRY_SUM32(n, x) ENTRY_SUM16 ((n), (x)) ENTRY_SUM16 ((n), (x)+16)
#define ENTRY_SUM64(n, x) ENTRY_SUM32 ((n), (x)) ENTRY_SUM32 ((n), (x)+32)
#define ENTRY_SUM128(n, x) ENTRY_SUM64 ((n), (x)) ENTRY_SUM64 ((n), (x)+64)

#define CASE_ENTRY(n) \
  case n: \
    sum = sum / (n + 1); \
    sum = restore (sum, n + 1); \
    if (sum == (n + addend)) \
      break;\
    ENTRY_SUM128 ((n), 2) \
    ENTRY_SUM16 ((n), 130) \
    break;

#define CASE_ENTRY2(n) CASE_ENTRY ((n)) CASE_ENTRY ((n)+1)
#define CASE_ENTRY4(n) CASE_ENTRY2 ((n)) CASE_ENTRY2 ((n)+2)
#define CASE_ENTRY8(n) CASE_ENTRY4 ((n)) CASE_ENTRY4 ((n)+4)
#define CASE_ENTRY16(n) CASE_ENTRY8 ((n)) CASE_ENTRY8 ((n)+8)
#define CASE_ENTRY32(n) CASE_ENTRY16 ((n)) CASE_ENTRY16 ((n)+16)
#define CASE_ENTRY64(n) CASE_ENTRY32 ((n)) CASE_ENTRY32 ((n)+32)
#define CASE_ENTRY128(n) CASE_ENTRY64 ((n)) CASE_ENTRY64 ((n)+64)

__attribute__((noinline, noclone)) long long
test_and_branch (int selector, int addend, int cond)
{
  long long sum = selector + 1;

  if (selector > 200)
    {
start0:
      return sum - 1;
start3:
      return sum - 2;
    }
  else
    {
      switch (selector)
	{
start1:
start2:
	  CASE_ENTRY128 (1)
	  CASE_ENTRY64 (129)
	  CASE_ENTRY16 (193)
	}

      do_nothing ((int *)&sum);

      if (cond == 0)
	goto start0;
      else if (cond < 0)
	goto start1;
      else if ((cond & 0x010) != 0)
	goto start2;
      else if (cond >= 14)
	goto start3;

    }

  return -1;
}

/* { dg-final { scan-assembler "Lbcond" } } */
/* { dg-final { scan-assembler "Lcb" } } */
/* { dg-final { scan-assembler "Ltb" } } */
