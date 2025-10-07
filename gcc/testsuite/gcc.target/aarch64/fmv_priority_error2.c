/* { dg-do compile }  */
/* { dg-require-ifunc } */
/* { dg-options "-std=c23 -pedantic-errors" } */

int foo [[gnu::target_version("dotprod;priority=0")]] (void); /* { dg-error "invalid feature modifier .priority=0. of value .dotprod;priority=0. in .target_version. attribute" } */
int foo [[gnu::target_version("dotprod;priority=-1")]] (void); /* { dg-error "invalid feature modifier .priority=-1. of value .dotprod;priority=-1. in .target_version. attribute" } */
int foo [[gnu::target_version("dotprod;priority=256")]] (void); /* { dg-error "invalid feature modifier .priority=256. of value .dotprod;priority=256. in .target_version. attribute" } */
int foo [[gnu::target_version("priority=4;dotprod")]] (void); /* { dg-error "invalid feature modifier .priority=4. of value .priority=4;dotprod. in .target_version. attribute" } */
