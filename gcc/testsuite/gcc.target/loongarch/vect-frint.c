/* { dg-do compile } */
/* { dg-options "-O2 -mabi=lp64d -mdouble-float -fno-math-errno -ffp-int-builtin-inexact -mlasx" } */

float out_x[8];
double out_y[4];

float x[8];
double y[4];

#define TEST(op, N, func) \
void \
test_##op##_##N##_##func () \
{ \
  for (int i = 0; i < N; i++) \
    out_##op[i] = __builtin_##func (op[i]); \
}

TEST(x, 4, ceilf);
TEST(x, 4, floorf);
TEST(x, 4, nearbyintf);
TEST(x, 4, rintf);
TEST(x, 4, roundf);
TEST(x, 4, roundevenf);
TEST(x, 4, truncf);

TEST(x, 8, ceilf);
TEST(x, 8, floorf);
TEST(x, 8, nearbyintf);
TEST(x, 8, rintf);
TEST(x, 8, roundf);
TEST(x, 8, roundevenf);
TEST(x, 8, truncf);

TEST(y, 2, ceil);
TEST(y, 2, floor);
TEST(y, 2, nearbyint);
TEST(y, 2, rint);
TEST(y, 2, round);
TEST(y, 2, roundeven);
TEST(y, 2, trunc);

TEST(y, 4, ceil);
TEST(y, 4, floor);
TEST(y, 4, nearbyint);
TEST(y, 4, rint);
TEST(y, 4, round);
TEST(y, 4, roundeven);
TEST(y, 4, trunc);

/* ceil */
/* { dg-final { scan-assembler "\tvfrintrp\.s" } } */
/* { dg-final { scan-assembler "\tvfrintrp\.d" } } */
/* { dg-final { scan-assembler "\txvfrintrp\.s" } } */
/* { dg-final { scan-assembler "\txvfrintrp\.d" } } */

/* floor */
/* { dg-final { scan-assembler "\tvfrintrm\.s" } } */
/* { dg-final { scan-assembler "\tvfrintrm\.d" } } */
/* { dg-final { scan-assembler "\txvfrintrm\.s" } } */
/* { dg-final { scan-assembler "\txvfrintrm\.d" } } */

/* rint and nearbyint
   nearbyint has been disallowed to raise FE_INEXACT for decades.  */
/* { dg-final { scan-assembler-times "\tvfrint\.s" 1 } } */
/* { dg-final { scan-assembler-times "\tvfrint\.d" 1 } } */
/* { dg-final { scan-assembler-times "\txvfrint\.s" 1 } } */
/* { dg-final { scan-assembler-times "\txvfrint\.d" 1 } } */
/* { dg-final { scan-assembler "bl\t%plt\\(nearbyint\\)" } } */
/* { dg-final { scan-assembler "bl\t%plt\\(nearbyintf\\)" } } */

/* round: we don't have a corresponding instruction */
/* { dg-final { scan-assembler "bl\t%plt\\(round\\)" } } */
/* { dg-final { scan-assembler "bl\t%plt\\(roundf\\)" } } */

/* roundeven */
/* { dg-final { scan-assembler "\tvfrintrne\.s" } } */
/* { dg-final { scan-assembler "\tvfrintrne\.d" } } */
/* { dg-final { scan-assembler "\txvfrintrne\.s" } } */
/* { dg-final { scan-assembler "\txvfrintrne\.d" } } */

/* trunc */
/* { dg-final { scan-assembler "\tvfrintrz\.s" } } */
/* { dg-final { scan-assembler "\tvfrintrz\.d" } } */
/* { dg-final { scan-assembler "\txvfrintrz\.s" } } */
/* { dg-final { scan-assembler "\txvfrintrz\.d" } } */
