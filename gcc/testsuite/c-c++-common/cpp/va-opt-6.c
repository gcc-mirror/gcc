/* { dg-do preprocess } */
/* { dg-options "-std=gnu99" { target c } } */
/* { dg-options "-std=c++20" { target c++ } } */

#define a ""
#define b(...) a ## #__VA_OPT__(1)	/* { dg-error "pasting \"a\" and \"\"\"\" does not give a valid preprocessing token" } */
#define c(...) a ## #__VA_OPT__(1)	/* { dg-error "pasting \"a\" and \"\"1\"\" does not give a valid preprocessing token" } */
#define d(...) #__VA_OPT__(1) ## !
#define e(...) #__VA_OPT__(1) ## !
#define f(...) #__VA_OPT__(. ## !)
#define g(...) #__VA_OPT__(. ## !)
b()
c(1)
d(   )		/* { dg-error "pasting \"\"\"\" and \"!\" does not give a valid preprocessing token" } */
e(  1 )		/* { dg-error "pasting \"\"1\"\" and \"!\" does not give a valid preprocessing token" } */
f()
g(0)		/* { dg-error "pasting \".\" and \"!\" does not give a valid preprocessing token" } */
