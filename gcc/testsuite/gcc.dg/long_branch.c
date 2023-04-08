/* { dg-do run } */
/* { dg-options "-O2 -fno-reorder-blocks" } */
/* { dg-skip-if "limited code space" { pdp11-*-* } } */
/* { dg-timeout-factor 2.0 { target hppa*-*-* } } */

void abort ();

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

#define CASE_ENTRY(n) \
  case n: \
    sum = sum / (n + 1); \
    sum = restore (sum, n + 1); \
    if (sum == (n + addend)) \
      break;\
    sum = sum / (n + 2); \
    sum = restore (sum, n + 2); \
    sum = sum / (n + 3); \
    sum = restore (sum, n + 3); \
    sum = sum / (n + 4); \
    sum = restore (sum, n + 4); \
    sum = sum / (n + 5); \
    sum = restore (sum, n + 5); \
    sum = sum / (n + 6); \
    sum = restore (sum, n + 6); \
    sum = sum / (n + 7); \
    sum = restore (sum, n + 7); \
    sum = sum / (n + 8); \
    sum = restore (sum, n + 8); \
    sum = sum / (n + 9); \
    sum = restore (sum, n + 9); \
    sum = sum / (n + 10); \
    sum = restore (sum, n + 10); \
    sum = sum / (n + 11); \
    sum = restore (sum, n + 11); \
    sum = sum / (n + 12); \
    sum = restore (sum, n + 12); \
    sum = sum / (n + 13); \
    sum = restore (sum, n + 13); \
    sum = sum / (n + 14); \
    sum = restore (sum, n + 14); \
    sum = sum / (n + 15); \
    sum = restore (sum, n + 15); \
    sum = sum / (n + 16); \
    sum = restore (sum, n + 16); \
    sum = sum / (n + 17); \
    sum = restore (sum, n + 17); \
    sum = sum / (n + 18); \
    sum = restore (sum, n + 18); \
    sum = sum / (n + 19); \
    sum = restore (sum, n + 19); \
    sum = sum / (n + 20); \
    sum = restore (sum, n + 20); \
    sum = sum / (n + 21); \
    sum = restore (sum, n + 21); \
    sum = sum / (n + 22); \
    sum = restore (sum, n + 22); \
    sum = sum / (n + 23); \
    sum = restore (sum, n + 23); \
    sum = sum / (n + 24); \
    sum = restore (sum, n + 24); \
    sum = sum / (n + 25); \
    sum = restore (sum, n + 25); \
    sum = sum / (n + 26); \
    sum = restore (sum, n + 26); \
    sum = sum / (n + 27); \
    sum = restore (sum, n + 27); \
    sum = sum / (n + 28); \
    sum = restore (sum, n + 28); \
    sum = sum / (n + 29); \
    sum = restore (sum, n + 29); \
    sum = sum / (n + 30); \
    sum = restore (sum, n + 30); \
    sum = sum / (n + 31); \
    sum = restore (sum, n + 31); \
    sum = sum / (n + 32); \
    sum = restore (sum, n + 32); \
    sum = sum / (n + 33); \
    sum = restore (sum, n + 33); \
    sum = sum / (n + 34); \
    sum = restore (sum, n + 34); \
    sum = sum / (n + 35); \
    sum = restore (sum, n + 35); \
    sum = sum / (n + 36); \
    sum = restore (sum, n + 36); \
    break;

__attribute__((noinline, noclone)) long long
test_and_branch (int selector, int addend)
{
  long long sum = selector + 1;

  if (selector > 64)
    {
start:
      return sum - 1;
    }
  else
    {
      switch (selector)
	{
	  CASE_ENTRY (1)
	  CASE_ENTRY (2)
	  CASE_ENTRY (3)
	  CASE_ENTRY (4)
	  CASE_ENTRY (5)
	  CASE_ENTRY (6)
	  CASE_ENTRY (7)
	  CASE_ENTRY (8)
	  CASE_ENTRY (9)
	  CASE_ENTRY (10)
	  CASE_ENTRY (11)
	  CASE_ENTRY (12)
	  CASE_ENTRY (13)
	  CASE_ENTRY (14)
	  CASE_ENTRY (15)
	  CASE_ENTRY (16)
	  CASE_ENTRY (17)
	  CASE_ENTRY (18)
	  CASE_ENTRY (19)
	  CASE_ENTRY (20)
	  CASE_ENTRY (21)
	  CASE_ENTRY (22)
	  CASE_ENTRY (23)
	  CASE_ENTRY (24)
	  CASE_ENTRY (25)
	  CASE_ENTRY (26)
	  CASE_ENTRY (27)
	  CASE_ENTRY (28)
	  CASE_ENTRY (29)
	  CASE_ENTRY (30)
	  CASE_ENTRY (31)
	  CASE_ENTRY (32)
	  CASE_ENTRY (33)
	  CASE_ENTRY (34)
	  CASE_ENTRY (35)
	  CASE_ENTRY (36)
	  CASE_ENTRY (37)
	  CASE_ENTRY (38)
	  CASE_ENTRY (39)
	  CASE_ENTRY (40)
	  CASE_ENTRY (41)
	  CASE_ENTRY (42)
	  CASE_ENTRY (43)
	  CASE_ENTRY (44)
	  CASE_ENTRY (45)
	  CASE_ENTRY (46)
	  CASE_ENTRY (47)
	  CASE_ENTRY (48)
	  CASE_ENTRY (49)
	  CASE_ENTRY (50)
	  CASE_ENTRY (51)
	  CASE_ENTRY (52)
	  CASE_ENTRY (53)
	  CASE_ENTRY (54)
	  CASE_ENTRY (55)
	  CASE_ENTRY (56)
	  CASE_ENTRY (57)
	  CASE_ENTRY (58)
	  CASE_ENTRY (59)
	  CASE_ENTRY (60)
	  CASE_ENTRY (61)
	  CASE_ENTRY (62)
	  CASE_ENTRY (63)
	  CASE_ENTRY (64)
	}

      do_nothing ((int *)&sum);

      if (sum & 0x40)
	goto start;
    }

  return -1;
}

int
main (int argc, char **argv)
{
  long long ret = test_and_branch (64, 1);
  if (ret != 64)
    abort ();

  ret = test_and_branch (7, 1);
  if (ret != -1)
    abort ();

  return 0;
}
