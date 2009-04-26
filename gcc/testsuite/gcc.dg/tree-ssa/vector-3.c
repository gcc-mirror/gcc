/* { dg-do compile } */
/* { dg-options "-w -O1 -fdump-tree-optimized" } */

#define vector __attribute((vector_size(16) ))
vector float a;

float f(float b)
{
  vector float c = {0, 0, 0, 0};
  vector float d = {0, 0, 0, 0};
  d += c;
  return ((float*)&c)[2];
}

/* We should be able to optimize this to just "return 0.0;" */
/* { dg-final { scan-tree-dump-times "BIT_FIELD_REF" 0 "optimized"} } */
/* { dg-final { scan-tree-dump-times "0.0" 1 "optimized"} } */

/* { dg-final { cleanup-tree-dump "optimized" } } */

