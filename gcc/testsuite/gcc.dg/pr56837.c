/* Limit this test to selected targets with IEEE double, 8-byte long long,
   supported 4x int vectors, 4-byte int.  */
/* { dg-do compile { target { i?86-*-* x86_64-*-* powerpc*-*-* } } } */
/* { dg-options "-O3 -fdump-tree-optimized" } */
/* { dg-additional-options "-msse2" { target ia32 } } */
/* { dg-additional-options "-mvsx -maltivec" { target powerpc*-*-* } } */

typedef int V __attribute__((__vector_size__ (16)));
#define N 1024
double d[N];
long long int l[N];
_Bool b[N];
_Complex double c[N];
V v[N];

void
fd (void)
{
  int i;
  for (i = 0; i < N; i++)
    d[i] = 747708026454360457216.0;
}

void
fl (void)
{
  int i;
  for (i = 0; i < N; i++)
    l[i] = 0x7c7c7c7c7c7c7c7cULL;
}

void
fb (void)
{
  int i;
  for (i = 0; i < N; i++)
    b[i] = 1;
}

void
fc (void)
{
  int i;
  for (i = 0; i < N; i++)
    c[i] = 747708026454360457216.0 + 747708026454360457216.0i;
}

void
fv (void)
{
  int i;
  for (i = 0; i < N; i++)
    v[i] = (V) { 0x12121212, 0x12121212, 0x12121212, 0x12121212 };
}

/* Look for
  __builtin_memset (&d, 68, 8192);
  __builtin_memset (&l, 124, 8192);
  __builtin_memset (&b, 1, 1024);
  __builtin_memset (&c, 68, 16384);
  __builtin_memset (&v, 18, 16384); */
/* { dg-final { scan-tree-dump-times "memset ..d, 68, 8192.;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "memset ..l, 124, 8192.;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "memset ..b, 1, 1024.;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "memset ..c, 68, 16384.;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "memset ..v, 18, 16384.;" 1 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
