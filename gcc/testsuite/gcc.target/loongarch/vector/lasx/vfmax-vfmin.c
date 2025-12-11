/* { dg-do compile } */
/* { dg-options "-O2 -mtune=la464 -mlasx" } */
/* { dg-final { scan-assembler "\tvfmin\\.d" } } */
/* { dg-final { scan-assembler "\tvfmax\\.d" } } */
/* { dg-final { scan-assembler "\txvfmin\\.d" } } */
/* { dg-final { scan-assembler "\txvfmax\\.d" } } */
/* { dg-final { scan-assembler "\tvfmin\\.s" } } */
/* { dg-final { scan-assembler "\tvfmax\\.s" } } */
/* { dg-final { scan-assembler "\txvfmin\\.s" } } */
/* { dg-final { scan-assembler "\txvfmax\\.s" } } */

#define T(OP) __typeof__ (__builtin_##OP (0, 0))

#define TEST(OP, LEN) \
void \
test_##OP##LEN (T (OP) *restrict dest, \
		const T (OP) *restrict src1, \
		const T (OP) *restrict src2) \
{ \
  for (int i = 0; i < LEN / sizeof (T(OP)); i++) \
    dest[i] = __builtin_##OP (src1[i], src2[i]); \
}

TEST(fmin, 16)
TEST(fmax, 16)
TEST(fmin, 32)
TEST(fmax, 32)
TEST(fminf, 16)
TEST(fmaxf, 16)
TEST(fminf, 32)
TEST(fmaxf, 32)
