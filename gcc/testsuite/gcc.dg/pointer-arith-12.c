/* { dg-options "-O2 -fdump-tree-optimized" } */

/* { dg-final { scan-tree-dump-not { & -16B?;} "optimized" } } */
/* { dg-final { scan-tree-dump-times { & 15;} 10 "optimized" } } */

typedef __UINTPTR_TYPE__ uintptr_t;

char *
f1 (char *x)
{
  char *y = x + 97;
  x += -((uintptr_t) y & 15);
  return x;
}

char *
f2 (char *x)
{
  char *y = x + 98;
  x += -((uintptr_t) y & 15);
  return x;
}

char *
f3 (char *x)
{
  char *y = x + 100;
  x += -((uintptr_t) y & 15);
  return x;
}

char *
f4 (char *x)
{
  char *y = x + 104;
  x += -((uintptr_t) y & 15);
  return x;
}

char *
f5 (char *x)
{
  x += 1 - ((uintptr_t) x & 15);
  return x;
}

char *
f6 (char *x)
{
  x += 2 - ((uintptr_t) x & 15);
  return x;
}

char *
f7 (char *x)
{
  x += 4 - ((uintptr_t) x & 15);
  return x;
}

char *
f8 (char *x)
{
  x += 8 - ((uintptr_t) x & 15);
  return x;
}

char *
f9 (char *x)
{
  char *y = x + 8;
  x += 16 - ((uintptr_t) y & 15);
  return x;
}

char *
f10 (char *x)
{
  char *y = x + 16;
  x += 8 - ((uintptr_t) y & 15);
  return x;
}
