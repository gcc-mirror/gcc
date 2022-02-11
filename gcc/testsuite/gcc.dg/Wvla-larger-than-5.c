/* PR middle-end/100510 - bogus -Wvla-large-than with -Walloca
   { dg-do compile }
   { dg-options "-O0 -Walloca -Wvla-larger-than=1000" }
   { dg-require-effective-target alloca } */

void f (void*);

#pragma GCC optimize ("0")

void nowarn_O0 (__SIZE_TYPE__ n)
{
  if (n > 32)
    return;

  char a[n];                  // { dg-bogus "\\\[-Wvla-larger-than=" }
  f (a);
}

#pragma GCC optimize ("1")

void nowarn_O1 (__SIZE_TYPE__ n)
{
  if (n > 33)
    return;

  char a[n];                  // { dg-bogus "\\\[-Wvla-larger-than=" }
  f (a);
}

#pragma GCC optimize ("2")

void nowarn_O2 (__SIZE_TYPE__ n)
{
  if (n > 34)
    return;

  char a[n];                  // { dg-bogus "\\\[-Wvla-larger-than=" }
  f (a);
}
