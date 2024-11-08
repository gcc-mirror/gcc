/* { dg-add-options vect_early_break } */
/* { dg-do compile } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */

/* { dg-additional-options "-Ofast" } */

/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" } } */

#include <stdbool.h>

#ifndef N
#define N 803
#endif
unsigned vect_b[N];
struct testStruct {
 long e;
 long f;
 bool a : 1;
 bool b : 1;
 int c : 14;
 int d;
};
struct testStruct vect_a[N];
  
unsigned test4(unsigned x)
{
 unsigned ret = 0;
 for (int i = 0; i < N; i++)
 {
   vect_b[i] = x + i;
   if (vect_a[i].a)
     return true;
   vect_a[i].e = x;
 }
 return ret;
}

