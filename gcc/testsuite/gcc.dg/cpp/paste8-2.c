/* { dg-do preprocess } */
/* { dg-options "-ftrack-macro-expansion=2" } */

int foo(int, ...);

#define a(x, y...) foo(x, ##y)
a(1)
a(1, 2, 3)
#define b(x, y, z...) foo(x, ##y) /* { dg-error "valid preprocessing token" } */
b(1, 2, 3)			
#define c(x, y, z...) foo(x, ##z)
c(1, 2)
c(1, 2, 3)
#define d(x) fo(##x) /* { dg-error "valid preprocessing token" } */
d(1)				
