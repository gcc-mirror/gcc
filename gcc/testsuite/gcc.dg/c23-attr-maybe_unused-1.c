/* Test C23 maybe_unused attribute: valid uses.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors -Wall -Wextra -Wunused" } */

[[maybe_unused]] static void f1 (void) {}

[[__maybe_unused__]] const int c1 = 1;
static int c2 [[maybe_unused]] = 2;

int
g ([[maybe_unused]] int x, int y)
{
  [[maybe_unused]] typedef float F;
  [[maybe_unused]] int a;
  int b [[__maybe_unused__]];
  int c [[maybe_unused]];
 [[__maybe_unused__]] label1:
  c = y;
 [[maybe_unused]] label2:
  return y;
}

struct [[maybe_unused]] s { double d; };

struct s2 { [[__maybe_unused__]] int a; int b [[maybe_unused]]; } x;

enum e { E1 [[maybe_unused]] };

union [[maybe_unused]] u { int x; };

enum [[maybe_unused]] eu { E2 };

union u2 { [[maybe_unused]] int a; int b [[maybe_unused]]; } y;

void
g2 (int x)
{
  switch (x)
    {
      [[maybe_unused]] case 1: ;
      [[__maybe_unused__]] case 2: ;
      [[maybe_unused]] default: ;
    }
}
