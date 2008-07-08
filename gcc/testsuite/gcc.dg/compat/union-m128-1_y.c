/* { dg-options "-O -msse2" } */

#include <stdlib.h>

#include "union-m128-1.h"

void
bar (SS_union_mi128 un)
{
  union_mi128 x;
  
  x.x = un.x;
  if (x.u [0] != 0x123456789abcedf0LL
      || x.u [1] != 0xfedcba9876543210LL)
    abort ();
}

void
foo (SS_struct_mi128 st)
{
  union_mi128 x;
  
  x.x = st.x;
  if (x.u [0] != 0x123456789abcedf0LL
      || x.u [1] != 0xfedcba9876543210LL)
    abort ();
}
