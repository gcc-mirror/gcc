/* Test C11 does not support empty initializers.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic-errors" } */

struct s { int a; };
struct s s = {}; /* { dg-error "empty initializer" } */
int x = {}; /* { dg-error "empty initializer" } */
float y = {}; /* { dg-error "empty initializer" } */
void *p = {}; /* { dg-error "empty initializer" } */
union u { int a; long b; };
union u z = {}; /* { dg-error "empty initializer" } */
int aa[2] = {}; /* { dg-error "empty initializer" } */

void
f (int a)
{
  int vla[a] = {}; /* { dg-error "empty initializer" } */
  struct s as = {}; /* { dg-error "empty initializer" } */
  int ax = {}; /* { dg-error "empty initializer" } */
  float ay = {}; /* { dg-error "empty initializer" } */
  void *ap = {}; /* { dg-error "empty initializer" } */
  union u az = {}; /* { dg-error "empty initializer" } */
  int aaa[2] = {}; /* { dg-error "empty initializer" } */
  int t = (int) {}; /* { dg-error "empty initializer" } */
}
