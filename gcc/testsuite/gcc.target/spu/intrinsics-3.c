/* { dg-do compile } */
#include <spu_intrinsics.h>
void f0 (vec_uint4 *in)
{
  vec_float4 out = spu_convtf (in[0], 128); /* { dg-error "expects an integer literal in the range" "0, 127"  }*/
}

void f1 (vec_int4 *in)
{
  vec_float4 out = spu_convtf (in[0], 128); /* { dg-error "expects an integer literal in the range" "0, 127"  }*/
}

void f2 (vec_float4 *in)
{
  vec_int4 out = spu_convts (in[0], 128); /* { dg-error "expects an integer literal in the range" "0, 127"  }*/
}

void f3 (vec_float4 *in)
{
  vec_uint4 out = spu_convtu (in[0], 128); /* { dg-error "expects an integer literal in the range" "0, 127"  }*/
}
