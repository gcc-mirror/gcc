/* PR sanitizer/81281 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-not "p_\[0-9]*\\(D\\)" "optimized" } } */

typedef __SIZE_TYPE__ size_t;
typedef __INTPTR_TYPE__ T;

T
f1 (char *p, size_t a, size_t b)
{
  char *c = p + 1;
  size_t d = a + 2;
  size_t e = b + 3;
  T f = (T) (p + a);
  T g = (T) (p + b);
  return f - g;
}

T
f2 (char *p, size_t a, size_t b)
{
  size_t c = a + 1;
  char *d = p + 2;
  size_t e = b + 3;
  T f = (T) (p + a);
  T g = (T) (p + b);
  return f - g;
}

T
f3 (char *p, size_t a, size_t b)
{
  size_t c = b + 1;
  char *d = p + 2;
  size_t e = a + 3;
  T f = (T) (p + a);
  T g = (T) (p + b);
  return f - g;
}

T
f4 (char *p, size_t a)
{
  char *c = p + 1;
  size_t d = a + 2;
  T f = (T) (p + a);
  T g = (T) p;
  return f - g;
}

T
f5 (char *p, size_t a)
{
  size_t c = a + 1;
  char *d = p + 2;
  T f = (T) (p + a);
  T g = (T) p;
  return f - g;
}

T
f6 (char *p, size_t a)
{
  char *c = p + 1;
  size_t d = a + 2;
  T f = (T) p;
  T g = (T) (p + a);
  return f - g;
}

T
f7 (char *p, size_t a)
{
  size_t c = a + 1;
  char *d = p + 2;
  T f = (T) p;
  T g = (T) (p + a);
  return f - g;
}
