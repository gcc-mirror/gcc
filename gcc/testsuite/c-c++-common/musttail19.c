/* { dg-do compile { target musttail } } */

float f1(void);

int f2(void)
{
  __attribute__((musttail)) return f1 (); /* { dg-error "changed after call" } */
}


int f3(int *);

int f4(int *p)
{
  int x;
  (void) p;
  __attribute__((musttail)) return f3(&x); /* { dg-warning "address of automatic variable 'x' passed to 'musttail' call argument" } */
}
