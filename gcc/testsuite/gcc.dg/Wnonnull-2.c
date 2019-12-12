/* PR middle-end/80936 - bcmp, bcopy, and bzero not declared nonnull
   Verify that -Wnonnull is issued for calls with constant null pointers
   with no optimization.
   { dg-do compile }
   { dg-options "-O0 -Wall" } */

void zero0 (void *p, unsigned n)
{
  __builtin_memset (0, 0, n);           // { dg-warning "\\\[-Wnonnull]" }
}

void zero1 (void *p, unsigned n)
{
  __builtin_bzero (0, n);               // { dg-warning "\\\[-Wnonnull]" }
}

void copy0 (void *p, const void *q, unsigned n)
{
  __builtin_memcpy (0, q, n);           // { dg-warning "\\\[-Wnonnull]" }
}

void copy1 (void *p, const void *q, unsigned n)
{
  __builtin_memcpy (0, q, n);           // { dg-warning "\\\[-Wnonnull]" }
}

void copy2 (void *p, const void *q, unsigned n)
{
  __builtin_bcopy (q, 0, n);            // { dg-warning "\\\[-Wnonnull]" }
}

void copy3 (void *p, const void *q, unsigned n)
{
  __builtin_bcopy (q, 0, n);            // { dg-warning "\\\[-Wnonnull]" }
}

int cmp0 (const void *p, const void *q, unsigned n)
{
  return __builtin_memcmp (0, q, n);    // { dg-warning "\\\[-Wnonnull]" }
}

int cmp1 (const void *p, const void *q, unsigned n)
{
  return __builtin_memcmp (0, q, n);    // { dg-warning "\\\[-Wnonnull]" }
}

int cmp2 (const void *p, const void *q, unsigned n)
{
  return __builtin_bcmp (0, q, n);      // { dg-warning "\\\[-Wnonnull]" }
}

int cmp3 (const void *p, const void *q, unsigned n)
{
  return __builtin_bcmp (p, 0, n);      // { dg-warning "\\\[-Wnonnull]" }
}
