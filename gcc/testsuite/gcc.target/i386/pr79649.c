/* PR tree-optimization/79649 */
/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-not "__builtin_memset" "optimized" } } */
/* { dg-final { scan-tree-dump-not "__builtin_memcpy" "optimized" } } */
/* { dg-final { scan-tree-dump-not "__builtin_memmove" "optimized" } } */

typedef __SIZE_TYPE__ size_t;

void
f1 (unsigned char __seg_gs *s, size_t n)
{
  for (size_t i = 0; i < n; ++i)
    s[i] = 0;
}

void
f2 (unsigned char __seg_gs *__restrict d, unsigned char __seg_gs *__restrict s, size_t n)
{
  for (size_t i = 0; i < n; ++i)
    d[i] = s[i];
}

void
f3 (unsigned char __seg_gs *__restrict d, unsigned char *__restrict s, size_t n)
{
  for (size_t i = 0; i < n; ++i)
    d[i] = s[i];
}

void
f4 (unsigned char *__restrict d, unsigned char __seg_gs *__restrict s, size_t n)
{
  for (size_t i = 0; i < n; ++i)
    d[i] = s[i];
}

void
f5 (unsigned char __seg_gs *__restrict d, unsigned char __seg_fs *__restrict s, size_t n)
{
  for (size_t i = 0; i < n; ++i)
    d[i] = s[i];
}

struct A { int a; char b[1024]; };
extern struct A __seg_gs a;

void
f6 (size_t n)
{
  for (size_t i = 0; i < n; ++i)
    a.b[i] = 0;
}
