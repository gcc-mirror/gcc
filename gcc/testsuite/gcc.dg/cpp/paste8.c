/* { dg-do preprocess } */
/* { dg-options "" } */

int foo(int, ...);

#define a(x, y...) foo(x, ##y)
a(1)
a(1, 2, 3)
#define b(x, y, z...) foo(x, ##y)
b(1, 2, 3)				/* { dg-warning "pasting would not" } */
#define c(x, y, z...) foo(x, ##z)
c(1, 2)
c(1, 2, 3)
#define d(x) foo(##x)			/* { dg-warning "nothing can be pasted" } */
d(1)
