/* { dg-additional-options "-O1" } */
/* { dg-additional-options "-mcpu=neoverse-n2" { target aarch64*-*-* } } */

#include "tree-vect.h"

typedef struct {
    int _M_current;
} __normal_iterator;

typedef struct {
    char _M_elems[5];
} array_5;

__normal_iterator __trans_tmp_1 = {-5};

__attribute__((noipa))
array_5 copySourceIntoTarget() {
    array_5 target;
    char* target_it = target._M_elems;

    while (__trans_tmp_1._M_current != 0) {
        *target_it = 1;
        __trans_tmp_1._M_current++;
        target_it++;
    }

    return target;
}

int main ()
{
  check_vect ();

  array_5 res = copySourceIntoTarget();

#pragma GCC novector
  for (int i = 0; i < 5; i++)
    if (res._M_elems[i] != 1)
      __builtin_abort ();
}
