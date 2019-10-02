/* PR middle-end/80936 - bcmp, bcopy, and bzero not declared nonnull
   Verify that with optimization, -Wnonnull is issued for calls with
   non-constant arguments determined to be null.
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

#define NOIPA __attribute__ ((noipa))

NOIPA void zero0 (void *p, unsigned n)
{
  if (p == 0)
    __builtin_memset (p, 0, n);         // { dg-warning "\\\[-Wnonnull]" }
}

NOIPA void zero1 (void *p, unsigned n)
{
  if (p == 0)
    __builtin_bzero (p, n);             // { dg-warning "\\\[-Wnonnull]" }
}

NOIPA void copy0 (void *p, const void *q, unsigned n)
{
  if (p == 0)
    __builtin_memcpy (p, q, n);         // { dg-warning "\\\[-Wnonnull]" }
}

NOIPA void copy1 (void *p, const void *q, unsigned n)
{
  if (q == 0)
    __builtin_memcpy (p, q, n);         // { dg-warning "\\\[-Wnonnull]" }
}

NOIPA void copy2 (void *p, const void *q, unsigned n)
{
  if (p == 0)
    __builtin_bcopy (q, p, n);          // { dg-warning "\\\[-Wnonnull]" }
}

NOIPA void copy3 (void *p, const void *q, unsigned n)
{
  if (q == 0)
    __builtin_bcopy (q, p, n);          // { dg-warning "\\\[-Wnonnull]" }
}

NOIPA int cmp0 (const void *p, const void *q, unsigned n)
{
  if (p == 0)
    return __builtin_memcmp (p, q, n);  // { dg-warning "\\\[-Wnonnull]" }
  return 0;
}

NOIPA int cmp1 (const void *p, const void *q, unsigned n)
{
  if (q == 0)
    return __builtin_memcmp (p, q, n);  // { dg-warning "\\\[-Wnonnull]" }
  return 0;
}

NOIPA int cmp2 (const void *p, const void *q, unsigned n)
{
  if (p == 0)
    return __builtin_bcmp (p, q, n);    // { dg-warning "\\\[-Wnonnull]" }
  return 0;
}

NOIPA int cmp3 (const void *p, const void *q, unsigned n)
{
  if (q == 0)
    return __builtin_bcmp (p, q, n);    // { dg-warning "\\\[-Wnonnull]" }
  return 0;
}
