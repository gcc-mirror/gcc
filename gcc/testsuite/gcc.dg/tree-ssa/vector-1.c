/* { dg-do compile } */
/* { dg-options "-w -O1 -fdump-tree-gimple" } */


/* We should be able to produce a BIT_FIELD_REF for each of these vector access. */
#define vector __attribute__((vector_size(16)))
float f0(vector float t)
{
  return ((float*)&t)[0];
}

float f1(vector float t)
{
  return ((float*)&t)[1];
}

float f2(vector float t)
{
  return ((float*)&t)[2];
}

float f3(vector float t)
{
  return ((float*)&t)[3];
}


/* { dg-final { scan-tree-dump-times "BIT_FIELD_REF" 4 "gimple"} } */

/* { dg-final { cleanup-tree-dump "gimple" } } */


