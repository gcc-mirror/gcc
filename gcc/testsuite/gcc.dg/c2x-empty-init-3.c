/* Test empty initializers diagnosed in C2X mode with -Wc11-c2x-compat.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -Wc11-c2x-compat" } */

struct s { int a; };
struct s s = {}; /* { dg-warning "empty initializer" } */
int x = {}; /* { dg-warning "empty initializer" } */
float y = {}; /* { dg-warning "empty initializer" } */
void *p = {}; /* { dg-warning "empty initializer" } */
union u { int a; long b; };
union u z = {}; /* { dg-warning "empty initializer" } */
int aa[2] = {}; /* { dg-warning "empty initializer" } */

void
f (int a)
{
  int vla[a] = {}; /* { dg-warning "empty initializer" } */
  struct s as = {}; /* { dg-warning "empty initializer" } */
  int ax = {}; /* { dg-warning "empty initializer" } */
  float ay = {}; /* { dg-warning "empty initializer" } */
  void *ap = {}; /* { dg-warning "empty initializer" } */
  union u az = {}; /* { dg-warning "empty initializer" } */
  int aaa[2] = {}; /* { dg-warning "empty initializer" } */
  int t = (int) {}; /* { dg-warning "empty initializer" } */
}
