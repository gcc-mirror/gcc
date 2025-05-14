/* PR middle-end/80936 - bcmp, bcopy, and bzero not declared nonnull
   Verify that with optimization, -Wnonnull is issued for calls with
   non-constant arguments determined to be null.
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

#define NOIPA __attribute__ ((noipa))

NOIPA void
zero0 (void *p, unsigned n)
{
  if (p == 0)
    __builtin_memset (p, 0, 0);
  if (p == 0)
    __builtin_memset (p, 0, n);
  if (p == 0 && n != 0)
    __builtin_memset (p, 0, n);         // { dg-warning "\\\[-Wnonnull]" }
}

NOIPA void
zero1 (void *p, unsigned n)
{
  if (p == 0)
    __builtin_bzero (p, 0);
  if (p == 0)
    __builtin_bzero (p, n);
  if (p == 0 && n != 0)
    __builtin_bzero (p, n);             // { dg-warning "\\\[-Wnonnull]" }
}

NOIPA void
copy0 (void *p, const void *q, unsigned n)
{
  if (p == 0)
    __builtin_memcpy (p, q, 0);
  if (p == 0)
    __builtin_memcpy (p, q, n);
  if (p == 0 && n != 0)
    __builtin_memcpy (p, q, n);         // { dg-warning "\\\[-Wnonnull]" }
}

NOIPA void
copy1 (void *p, const void *q, unsigned n)
{
  if (q == 0)
    __builtin_memcpy (p, q, 0);
  if (q == 0)
    __builtin_memcpy (p, q, n);
  if (q == 0 && n != 0)
    __builtin_memcpy (p, q, n);         // { dg-warning "\\\[-Wnonnull]" }
}

NOIPA void
copy2 (void *p, const void *q, unsigned n)
{
  if (p == 0)
    __builtin_bcopy (q, p, 0);
  if (p == 0)
    __builtin_bcopy (q, p, n);
  if (p == 0 && n != 0)
    __builtin_bcopy (q, p, n);          // { dg-warning "\\\[-Wnonnull]" }
}

NOIPA void
copy3 (void *p, const void *q, unsigned n)
{
  if (q == 0)
    __builtin_bcopy (q, p, 0);
  if (q == 0)
    __builtin_bcopy (q, p, n);
  if (q == 0 && n != 0)
    __builtin_bcopy (q, p, n);          // { dg-warning "\\\[-Wnonnull]" }
}

NOIPA int
cmp0 (const void *p, const void *q, unsigned n)
{
  if (p == 0)
    return __builtin_memcmp (p, q, 0);
  return 0;
}

NOIPA int
cmp1 (const void *p, const void *q, unsigned n)
{
  if (q == 0)
    return __builtin_memcmp (p, q, 0);
  return 0;
}

NOIPA int
cmp2 (const void *p, const void *q, unsigned n)
{
  if (p == 0)
    return __builtin_bcmp (p, q, 0);
  return 0;
}

NOIPA int
cmp3 (const void *p, const void *q, unsigned n)
{
  if (q == 0)
    return __builtin_bcmp (p, q, 0);
  return 0;
}

NOIPA int
cmp4 (const void *p, const void *q, unsigned n)
{
  if (p == 0)
    return __builtin_memcmp (p, q, n);
  return 0;
}

NOIPA int
cmp5 (const void *p, const void *q, unsigned n)
{
  if (q == 0)
    return __builtin_memcmp (p, q, n);
  return 0;
}

NOIPA int
cmp6 (const void *p, const void *q, unsigned n)
{
  if (p == 0)
    return __builtin_bcmp (p, q, n);
  return 0;
}

NOIPA int
cmp7 (const void *p, const void *q, unsigned n)
{
  if (q == 0)
    return __builtin_bcmp (p, q, n);
  return 0;
}

NOIPA int
cmp8 (const void *p, const void *q, unsigned n)
{
  if (p == 0 && n != 0)
    return __builtin_memcmp (p, q, n);  // { dg-warning "\\\[-Wnonnull]" }
  return 0;
}

NOIPA int
cmp9 (const void *p, const void *q, unsigned n)
{
  if (q == 0 && n != 0)
    return __builtin_memcmp (p, q, n);  // { dg-warning "\\\[-Wnonnull]" }
  return 0;
}

NOIPA int
cmp10 (const void *p, const void *q, unsigned n)
{
  if (p == 0 && n != 0)
    return __builtin_bcmp (p, q, n);    // { dg-warning "\\\[-Wnonnull]" }
  return 0;
}

NOIPA int
cmp11 (const void *p, const void *q, unsigned n)
{
  if (q == 0 && n != 0)
    return __builtin_bcmp (p, q, n);    // { dg-warning "\\\[-Wnonnull]" }
  return 0;
}
