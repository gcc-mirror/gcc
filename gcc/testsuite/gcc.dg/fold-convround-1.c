/* PR middle-end/28473.  */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-original" } */

extern double round(double);
extern double floor(double);
extern double ceil(double);

unsigned long long test1(double x)
{
  return (unsigned long long) round(x);
}

unsigned long long test2(double x)
{
  return (unsigned long long) floor(x);
}
unsigned long long test3(double x)
{
  return (unsigned long long) ceil(x);
}

/* { dg-final { scan-tree-dump-times "__builtin_lround" 0 "original" } } */
/* { dg-final { scan-tree-dump-times "__builtin_llround" 0 "original" } } */
/* { dg-final { scan-tree-dump-times "__builtin_lfloor" 0 "original" } } */
/* { dg-final { scan-tree-dump-times "__builtin_llfloor" 0 "original" } } */
/* { dg-final { scan-tree-dump-times "__builtin_lceil" 0 "original" } } */
/* { dg-final { scan-tree-dump-times "__builtin_llceil" 0 "original" } } */
/* { dg-final { cleanup-tree-dump "original" } } */

