/* { dg-do compile { target { musttail && { c || c++11 } } } } */

float f1(void);

int f2(void)
{
  [[gnu::musttail]] return f1 (); /* { dg-error "changed after call" } */
}


int f3(int *);

int f4(void)
{
  int x;
  [[gnu::musttail]] return f3(&x); /* { dg-error "\(refers to locals|other reasons\)" } */
}
