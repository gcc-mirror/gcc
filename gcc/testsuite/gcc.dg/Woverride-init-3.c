/* Test for warnings for overriding designated initializers: not
   warned for with -Wextra -Wno-override-init.  Bug 24010.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-Wextra -Wno-override-init" } */

struct s { int a; int b; int c; };
union u { char a; long long b; };

struct s s0 = {
  .a = 1,
  .b = 2,
  .a = 3,
  4,
  5
};

union u u0 = {
  .a = 1,
  .b = 2,
  .a = 3
};

int a[5] = {
  [0] = 1,
  [1] = 2,
  [0] = 3,
  [2] = 4
};
