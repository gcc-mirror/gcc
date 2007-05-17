/* { dg-do preprocess } */
/* { dg-options "" } */

int foo(int, ...);

#define a(x, y...) foo(x, ##y)
a(1)
a(1, 2, 3)
#define b(x, y, z...) foo(x, ##y)
b(1, 2, 3)			/* { dg-error "valid preprocessing token" } */
#define c(x, y, z...) foo(x, ##z)
c(1, 2)
c(1, 2, 3)
#define d(x) fo(##x)
d(1)				/* { dg-error "valid preprocessing token" } */
