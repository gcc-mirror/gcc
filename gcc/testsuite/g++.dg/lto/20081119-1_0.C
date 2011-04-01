/* { dg-lto-do link } */
/* { dg-require-effective-target fpic } */
/* { dg-lto-options {{-fPIC -flto -flto-partition=1to1 -r -nostdlib}} } */

#include "20081119-1.h"

extern __gnu_cxx::new_allocator<int> X;

int
f (__gnu_cxx::new_allocator<int> * a)
{
 return a->max_size () + X.max_size();
}
