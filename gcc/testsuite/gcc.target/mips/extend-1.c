/* { dg-options "-O -mgp64 forbid_cpu=octeon.*" } */
/* { dg-final { scan-assembler-times "\tdsll\t" 5 } } */
/* { dg-final { scan-assembler-times "\tdsra\t" 5 } } */
/* { dg-final { scan-assembler-not "\tsll\t" } } */

#define TEST_CHAR(T, N)	\
  NOMIPS16 T f##N (long long d, T *a, T *r) { T b = (char) d;  *r = b + *a; }
#define TEST_SHORT(T, N) \
  NOMIPS16 T g##N (long long d, T *a, T *r) { T b = (short) d; *r = b + *a; }
#define TEST(T, N) TEST_CHAR (T, N) TEST_SHORT (T, N)

TEST (int, 1);
TEST (long long, 2);
TEST_CHAR (short, 3);
