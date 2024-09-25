/* { dg-do compile } */
/* { dg-require-effective-target int32 } */
/* { dg-options "-O2 -fdump-tree-pre-details" } */
/* { dg-additional-options "-msse -mfpmath=sse" { target { x86_64-*-* i?86-*-* } } } */

int f;
int g;
unsigned int
foo (int b, int x)
{
  if (b)
    x = *(int *)&f;
  g = x;
  return *(unsigned int*)&f;
}
float
bar (int b, int x)
{
  if (b)
    x = *(int *)&f;
  g = x;
  return *(float *)&f;
}

/* We should see the partial redundant loads of f even though they
   are using different types (of the same size).  */

/* { dg-final { scan-tree-dump-times "Replaced MEM" 3 "pre" } } */
