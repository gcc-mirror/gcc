/* { dg-options "-O2 -fdump-tree-optimized" } */

/* { dg-final { scan-tree-dump-times { & -16B?;} 4 "optimized" { target lp64 } } } */
/* { dg-final { scan-tree-dump-times { \+ 16;} 3 "optimized" } } */
/* { dg-final { scan-tree-dump-not { & 15;} "optimized" } } */
/* { dg-final { scan-tree-dump-not { \+ 96;} "optimized" } } */

typedef __UINTPTR_TYPE__ uintptr_t;

char *
f1 (char *x)
{
  char *y = x + 32;
  x += -((uintptr_t) y & 15);
  return x;
}

char *
f2 (char *x)
{
  x += 16 - ((uintptr_t) x & 15);
  return x;
}

char *
f3 (char *x)
{
  char *y = x + 32;
  x += 16 - ((uintptr_t) y & 15);
  return x;
}

char *
f4 (char *x)
{
  char *y = x + 16;
  x += 16 - ((uintptr_t) y & 15);
  return x;
}
