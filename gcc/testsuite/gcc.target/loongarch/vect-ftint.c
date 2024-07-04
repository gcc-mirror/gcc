/* { dg-do compile } */
/* { dg-options "-O2 -mabi=lp64d -mdouble-float -fno-math-errno -ffp-int-builtin-inexact -mlasx" } */

int out_x[8];
long out_y[4];

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
/* { dg-final { scan-assembler "\tvftintrp\.w\.s" } } */
/* { dg-final { scan-assembler "\tvftintrp\.l\.d" } } */
/* { dg-final { scan-assembler "\txvftintrp\.w\.s" } } */
/* { dg-final { scan-assembler "\txvftintrp\.l\.d" } } */

/* floor */
/* { dg-final { scan-assembler "\tvftintrm\.w\.s" } } */
/* { dg-final { scan-assembler "\tvftintrm\.l\.d" } } */
/* { dg-final { scan-assembler "\txvftintrm\.w\.s" } } */
/* { dg-final { scan-assembler "\txvftintrm\.l\.d" } } */

/* rint and nearbyint
   nearbyint has been disallowed to raise FE_INEXACT for decades.  */
/* { dg-final { scan-assembler-times "\tvftint\.w\.s" 1 } } */
/* { dg-final { scan-assembler-times "\tvftint\.l\.d" 1 } } */
/* { dg-final { scan-assembler-times "\txvftint\.w\.s" 1 } } */
/* { dg-final { scan-assembler-times "\txvftint\.l\.d" 1 } } */
/* { dg-final { scan-assembler "bl\t%plt\\(nearbyint\\)" } } */
/* { dg-final { scan-assembler "bl\t%plt\\(nearbyintf\\)" } } */

/* round: we don't have a corresponding instruction */
/* { dg-final { scan-assembler "bl\t%plt\\(lround\\)" } } */
/* { dg-final { scan-assembler "bl\t%plt\\(roundf\\)" } } */

/* roundeven */
/* { dg-final { scan-assembler "\tvftintrne\.w\.s" } } */
/* { dg-final { scan-assembler "\tvftintrne\.l\.d" } } */
/* { dg-final { scan-assembler "\txvftintrne\.w\.s" } } */
/* { dg-final { scan-assembler "\txvftintrne\.l\.d" } } */

/* trunc */
/* { dg-final { scan-assembler-not "bl\t%plt\\(trunc\\)" } } */
/* { dg-final { scan-assembler-not "bl\t%plt\\(truncf\\)" } } */
