/* PR middle-end/80936 - bcmp, bcopy, and bzero not declared nonnull
   Verify that -Wnonnull is issued for calls with constant null pointers
   with no optimization.
   { dg-do compile }
   { dg-options "-O0 -Wall" } */

void
zero0 (void *p, unsigned n)
{
  __builtin_memset (0, 0, 0);
  __builtin_memset (0, 0, n);
  __builtin_memset (0, 0, 1);           // { dg-warning "\\\[-Wnonnull]" }
}

void
zero1 (void *p, unsigned n)
{
  __builtin_bzero (0, 0);
  __builtin_bzero (0, n);
  __builtin_bzero (0, 2);               // { dg-warning "\\\[-Wnonnull]" }
}

void
copy0 (void *p, const void *q, unsigned n)
{
  __builtin_memcpy (0, q, 0);
  __builtin_memcpy (0, q, n);
  __builtin_memcpy (0, q, 3);           // { dg-warning "\\\[-Wnonnull]" }
}

void
copy1 (void *p, const void *q, unsigned n)
{
  __builtin_memcpy (p, 0, 0);
  __builtin_memcpy (p, 0, n);
  __builtin_memcpy (p, 0, 4);           // { dg-warning "\\\[-Wnonnull]" }
}

void
copy2 (void *p, const void *q, unsigned n)
{
  __builtin_bcopy (0, p, 0);
  __builtin_bcopy (0, p, n);
  __builtin_bcopy (0, p, 5);            // { dg-warning "\\\[-Wnonnull]" }
}

void
copy3 (void *p, const void *q, unsigned n)
{
  __builtin_bcopy (q, 0, 0);
  __builtin_bcopy (q, 0, n);
  __builtin_bcopy (q, 0, 6);            // { dg-warning "\\\[-Wnonnull]" }
}

int
cmp0 (const void *p, const void *q, unsigned n)
{
  return __builtin_memcmp (0, q, 0);
}

int
cmp1 (const void *p, const void *q, unsigned n)
{
  return __builtin_memcmp (0, q, 0);
}

int
cmp2 (const void *p, const void *q, unsigned n)
{
  return __builtin_bcmp (0, q, 0);
}

int
cmp3 (const void *p, const void *q, unsigned n)
{
  return __builtin_bcmp (p, 0, 0);
}

int
cmp4 (const void *p, const void *q, unsigned n)
{
  return __builtin_memcmp (0, q, n);
}

int
cmp5 (const void *p, const void *q, unsigned n)
{
  return __builtin_memcmp (0, q, n);
}

int
cmp6 (const void *p, const void *q, unsigned n)
{
  return __builtin_bcmp (0, q, n);
}

int
cmp7 (const void *p, const void *q, unsigned n)
{
  return __builtin_bcmp (p, 0, n);
}

int
cmp8 (const void *p, const void *q, unsigned n)
{
  return __builtin_memcmp (0, q, 41);    // { dg-warning "\\\[-Wnonnull]" }
}

int
cmp9 (const void *p, const void *q, unsigned n)
{
  return __builtin_memcmp (0, q, 42);    // { dg-warning "\\\[-Wnonnull]" }
}

int
cmp10 (const void *p, const void *q, unsigned n)
{
  return __builtin_bcmp (0, q, 43);      // { dg-warning "\\\[-Wnonnull]" }
}

int
cmp11 (const void *p, const void *q, unsigned n)
{
  return __builtin_bcmp (p, 0, 44);      // { dg-warning "\\\[-Wnonnull]" }
}
