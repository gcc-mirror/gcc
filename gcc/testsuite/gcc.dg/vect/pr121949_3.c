#ifndef TYPE
#define TYPE long long
#define MAX  64
#define IV_TYPE int
#endif

#include "tree-vect.h"

__attribute__((noipa))
void f(TYPE* acc)
{
    for (IV_TYPE row = 0; row < MAX; ++row)
      acc[row] = acc[row] << row;
}

__attribute__((noipa))
void g(TYPE* acc)
{
#pragma GCC novector
    for (IV_TYPE row = 0; row < MAX; ++row)
      acc[row] = acc[row] << row;
}

int main ()
{

   check_vect ();

   TYPE acc1[MAX] = {};
   TYPE acc2[MAX] = {};
#pragma GCC novector
   for (int i = 0; i < MAX; i++)
     acc1[i] = acc2[i] = i;

  f (acc1);
  f (acc2);

#pragma GCC novector
   for (int i = 0; i < MAX; i++)
    if (acc1[i] != acc2[i])
        __builtin_abort ();
}

/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" { target { vect_var_shift && vect_int } } } }  */
/* { dg-final { scan-tree-dump "vect_recog_vector_vector_shift_pattern: detected" "vect" { target { vect_var_shift && vect_int } } } }  */
