/* Verify that VLA definitions with an unknown upper bound don't trigger
   -Wvla-larger-than= warnings by default.
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

void f (void *, ...);

void nowarn_vla_int (int n)
{
  char a[n];

  if (n < 1234)
    n = 1234;

  char b[n];
  f (a, b);
}

void nowarn_vla_uint (unsigned n)
{
  char a[n];
  f (a);

  if (n < 2345)
    n = 2345;

  char b[n];
  f (a, b);
}

void nowarn_vla_long (long n)
{
  char a[n];

  if (n < 1234)
    n = 1234;

  char b[n];
  f (a, b);
}

void nowarn_vla_ulong (unsigned long n)
{
  char a[n];
  f (a);

  if (n < 2345)
    n = 2345;

  char b[n];
  f (a, b);
}

/* Verify that a VLA whose size is definitely in excess of PTRDIFF_MAX
   is diagnosed by default.  */

void warn_vla (__PTRDIFF_TYPE__ n)
{
  int a[n];
  f (a);

  if (n <= __PTRDIFF_MAX__)
    n = __PTRDIFF_MAX__;

  int b[n];   /* { dg-warning "argument to variable-length array is too large" } */
  f (a, b);
}
