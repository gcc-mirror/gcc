/* { dg-do compile { target { ! x32 } } } */
/* { dg-options "-fcheck-pointer-bounds -mmpx" } */

int foo(int);

typedef struct {
  double d;
  int a;
} str_t;

void bar(double d, int i, str_t s)
{
  d = ((double (*) (int)) foo) (i); /* { dg-warning "function called through a non-compatible type" } */
}
