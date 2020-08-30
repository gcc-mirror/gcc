/* Verify that calling strndup and strnlen with an unknown bound isn't
   diagnosed regardless of the size of the array and the type of the bound.
  { dg-do compile }
  { dg-options "-O -Wall" } */

#define NOIPA __attribute__ ((noipa))

typedef __SIZE_TYPE__ size_t;

extern char* strndup (const char*, size_t);
extern size_t strnlen (const char*, size_t);

/* TO DO: Passing a zero-length array to any function is almost certainly
   a bug and should be diagnosed except perpaphs when the function also
   takes a bound and its value is known to be zero.  When this is
   implemented this test will need to be adjusted.  */
extern char a0[0];

extern char a1[1];

NOIPA char* strndup_a0_si (short n)
{
  return strndup (a0, n);
}

NOIPA char* strndup_a0_i (int n)
{
  return strndup (a0, n);     // { dg-bogus "\\\[-Wstringop-overread" }
}

NOIPA char* strndup_a0_li (long n)
{
  return strndup (a0, n);     // { dg-bogus "\\\[-Wstringop-overread" }
}

NOIPA char* strndup_a0_lli (long long n)
{
  return strndup (a0, n);     // { dg-bogus "\\\[-Wstringop-overread" }
}


NOIPA char* strndup_a0_usi (unsigned short n)
{
  return strndup (a0, n);
}

NOIPA char* strndup_a0_ui (unsigned n)
{
  return strndup (a0, n);     // { dg-bogus "\\\[-Wstringop-overread" }
}

NOIPA char* strndup_a0_uli (unsigned long n)
{
  return strndup (a0, n);     // { dg-bogus "\\\[-Wstringop-overread" }
}

NOIPA char* strndup_a0_ulli (unsigned long long n)
{
  return strndup (a0, n);     // { dg-bogus "\\\[-Wstringop-overread" }
}



NOIPA char* strndup_a1_si (short n)
{
  return strndup (a1, n);
}

NOIPA char* strndup_a1_i (int n)
{
  return strndup (a1, n);     // { dg-bogus "\\\[-Wstringop-overread" }
}

NOIPA char* strndup_a1_li (long n)
{
  return strndup (a1, n);     // { dg-bogus "\\\[-Wstringop-overread" }
}

NOIPA char* strndup_a1_lli (long long n)
{
  return strndup (a1, n);     // { dg-bogus "\\\[-Wstringop-overread" }
}


NOIPA char* strndup_a1_usi (unsigned short n)
{
  return strndup (a1, n);
}

NOIPA char* strndup_a1_ui (unsigned n)
{
  return strndup (a1, n);     // { dg-bogus "\\\[-Wstringop-overread" }
}

NOIPA char* strndup_a1_uli (unsigned long n)
{
  return strndup (a1, n);     // { dg-bogus "\\\[-Wstringop-overread" }
}

NOIPA char* strndup_a1_ulli (unsigned long long n)
{
  return strndup (a1, n);     // { dg-bogus "\\\[-Wstringop-overread" }
}


NOIPA size_t strnlen_a0_si (short n)
{
  return strnlen (a0, n);
}

NOIPA size_t strnlen_a0_i (int n)
{
  return strnlen (a0, n);     // { dg-bogus "\\\[-Wstringop-overread" }
}

NOIPA size_t strnlen_a0_li (long n)
{
  return strnlen (a0, n);     // { dg-bogus "\\\[-Wstringop-overread" }
}

NOIPA size_t strnlen_a0_lli (long long n)
{
  return strnlen (a0, n);     // { dg-bogus "\\\[-Wstringop-overread" }
}


NOIPA size_t strnlen_a0_usi (unsigned short n)
{
  return strnlen (a0, n);
}

NOIPA size_t strnlen_a0_ui (unsigned n)
{
  return strnlen (a0, n);     // { dg-bogus "\\\[-Wstringop-overread" }
}

NOIPA size_t strnlen_a0_uli (unsigned long n)
{
  return strnlen (a0, n);     // { dg-bogus "\\\[-Wstringop-overread" }
}

NOIPA size_t strnlen_a0_ulli (unsigned long long n)
{
  return strnlen (a0, n);     // { dg-bogus "\\\[-Wstringop-overread" }
}



NOIPA size_t strnlen_a1_si (short n)
{
  return strnlen (a1, n);
}

NOIPA size_t strnlen_a1_i (int n)
{
  return strnlen (a1, n);     // { dg-bogus "\\\[-Wstringop-overread" }
}

NOIPA size_t strnlen_a1_li (long n)
{
  return strnlen (a1, n);     // { dg-bogus "\\\[-Wstringop-overread" }
}

NOIPA size_t strnlen_a1_lli (long long n)
{
  return strnlen (a1, n);     // { dg-bogus "\\\[-Wstringop-overread" }
}


NOIPA size_t strnlen_a1_usi (unsigned short n)
{
  return strnlen (a1, n);
}

NOIPA size_t strnlen_a1_ui (unsigned n)
{
  return strnlen (a1, n);     // { dg-bogus "\\\[-Wstringop-overread" }
}

NOIPA size_t strnlen_a1_uli (unsigned long n)
{
  return strnlen (a1, n);     // { dg-bogus "\\\[-Wstringop-overread" }
}

NOIPA size_t strnlen_a1_ulli (unsigned long long n)
{
  return strnlen (a1, n);     // { dg-bogus "\\\[-Wstringop-overread" }
}
