/* { dg-do compile } */
/* { dg-options "-w -O1 -fdump-tree-optimized" } */
/* { dg-options "-w -O1 -fdump-tree-optimized -msse" { target { i?86-*-* x86_64-*-* } } } */

#define vector __attribute__(( vector_size(16) ))

float f(vector float a, int b, vector float c)
{
  vector float dd = c*a;
  a = (vector float){0,0,0,0};
  c = (vector float){0,0,0,0};
  {
   float d = ((float*)&a)[0];
   float d1 = ((float*)&c)[0];
   return d*d1;
  }
}

/* We should be able to optimize this to just "return 0.0;" */
/* { dg-final { scan-tree-dump-not "BIT_FIELD_REF" "optimized"} } */
/* { dg-final { scan-tree-dump-times "return 0.0" 1 "optimized"} } */

