/* PR middle-end/82646 - bogus -Wstringop-overflow with -D_FORTIFY_SOURCE=2
   on strncpy with range to a member array
   { dg-do compile }
   { dg-options "-O2 -Wstringop-overflow -ftrack-macro-expansion=0" } */

#define bos(p)   __builtin_object_size (p, 1)

struct S {
  char a[5];
  void (*pf)(void);
};

/* Verify that none of the string function calls below triggers a warning.  */

char* test_stpncpy_const_nowarn (struct S *p)
{
  int n = sizeof p->a;

  return __builtin_stpncpy (p->a, "123456", n);
}

char* test_strncpy_const_nowarn (struct S *p)
{
  int n = sizeof p->a;

  return __builtin_strncpy (p->a, "1234567", n);
}

char* test_stpncpy_chk_const_nowarn (struct S *p)
{
  int n = sizeof p->a;

  return __builtin___stpncpy_chk (p->a, "12345678", n, bos (p->a));
}

char* test_strncpy_chk_const_nowarn (struct S *p)
{
  int n = sizeof p->a;

  return __builtin___strncpy_chk (p->a, "123456789", n, bos (p->a));
}


char* test_stpncpy_range_nowarn (struct S *p, int n)
{
  if (n < sizeof p->a)
    n = sizeof p->a;

  return __builtin_stpncpy (p->a, "123456", n);
}

char* test_strncpy_range_nowarn (struct S *p, int n)
{
  if (n < sizeof p->a)
    n = sizeof p->a;

  return __builtin_strncpy (p->a, "1234567", n);
}

char* test_stpncpy_chk_range_nowarn (struct S *p, int n)
{
  if (n < sizeof p->a)
    n = sizeof p->a;

  return __builtin___stpncpy_chk (p->a, "12345678", n, bos (p->a));   /* { dg-bogus "\\\[-Wstringop-overflow=]" } */
}

char* test_strncpy_chk_range_nowarn (struct S *p, int n)
{
  if (n < sizeof p->a)
    n = sizeof p->a;

  return __builtin___strncpy_chk (p->a, "123456789", n, bos (p->a));  /* { dg-bogus "\\\[-Wstringop-overflow=]" } */
}


/* Verify that all of the string function calls below trigger a warning.  */

char* test_stpncpy_const_warn (struct S *p)
{
  int n = sizeof p->a;

  ++n;

  return __builtin_stpncpy (p->a, "123456", n);                       /* { dg-warning "\\\[-Wstringop-overflow=]" } */
}

char* test_strncpy_const_warn (struct S *p)
{
  int n = sizeof p->a;

  /* A call to strncpy() with a known string and small bound is folded
     into memcpy() which defeats the warning in this case since memcpy
     uses Object Size Type 0, i.e., the largest object that p->a may
     be a part of.  Use a larger bound to get around this here.  */
  n += 11;

  return __builtin_strncpy (p->a, "1234567", n);                      /* { dg-warning "\\\[-Wstringop-overflow=]" } */
}

char* test_stpncpy_chk_const_warn (struct S *p)
{
  int n = sizeof p->a;

  ++n;

  return __builtin___stpncpy_chk (p->a, "12345678", n, bos (p->a));   /* { dg-warning "\\\[-Wstringop-overflow=]" } */
}

char* test_strncpy_chk_const_warn (struct S *p)
{
  int n = sizeof p->a;

  ++n;

  return __builtin___strncpy_chk (p->a, "123456789", n, bos (p->a));  /* { dg-warning "\\\[-Wstringop-overflow=]" } */
}


char* test_stpncpy_range_warn (struct S *p, int n)
{
  if (n < sizeof p->a + 1)
    n = sizeof p->a + 1;

  return __builtin_stpncpy (p->a, "123456", n);                       /* { dg-warning "\\\[-Wstringop-overflow=]" } */
}

char* test_strncpy_range_warn (struct S *p, int n)
{
  if (n < sizeof p->a + 1)
    n = sizeof p->a + 1;

  return __builtin_strncpy (p->a, "1234567", n);                      /* { dg-warning "\\\[-Wstringop-overflow=]" } */
}

char* test_stpncpy_chk_range_warn (struct S *p, int n)
{
  if (n < sizeof p->a + 1)
    n = sizeof p->a + 1;

  return __builtin___stpncpy_chk (p->a, "12345678", n, bos (p->a));   /* { dg-warning "\\\[-Wstringop-overflow=]" } */
}

char* test_strncpy_chk_range_warn (struct S *p, int n)
{
  if (n < sizeof p->a + 1)
    n = sizeof p->a + 1;

  return __builtin___strncpy_chk (p->a, "123456789", n, bos (p->a));  /* { dg-warning "\\\[-Wstringop-overflow=]" } */
}
