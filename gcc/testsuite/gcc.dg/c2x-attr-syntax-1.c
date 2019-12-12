/* Test C2x attribute syntax.  Basic tests of valid uses of empty
   attributes.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -pedantic-errors" } */

[ [ ] ] [[]];

[[]] int [[]] a [[]] = 123;

int f([[]] int x [[]], [[]] long [[]], short [[]] *[[]] [3] [[]],
      int [[]] (int)[[]], int (*)(int)[[]]) [[]] [[]];

int g [[]] [2] [[]] [3] [[]];

int *[[]] const *[[]] volatile *[[]] *const p;

int *[[]][[]] q = 0;

struct [[]] s;
union [[]][[]] u;

struct [[]] s2 { [[]] long [[]] *a[[]] [3] [[]] [4], b[[]]; };

union [[]] u2 { [[]] long [[]] *a[[]] [3] [[]] [4]; };

int z = sizeof (int [[]]);

enum [[]] { E1 [[]][[]], E2[[]][[]] = 3 };
enum [[]] e { E3 = 4, E4 [[]] };

void
func (void) [[]]
{
  [[]] int var;
  [[]] { }
  [[]] switch (a) { [[]] case 1: [[]] case 2: [[]] default: [[]] var = 3; }
  [[]] x : [[]] y: [[]] var = 1;
  [[]];
  int [[]] var2;
  [[]] if (a) [[]] (void) 0; else [[]] (void) 1;
  [[]] while (0) [[]] var = 2;
  [[]] do [[]] var = 3; while (0);
  for ([[]] int zz = 1; zz < 10; zz++)
    {
      [[]] var2 = 8;
      [[]] continue;
      [[]] break;
    }
  if (a) [[]] goto x;
  [[]] return;
}

void func2 () [[]];

void func3 () [[]] { }
