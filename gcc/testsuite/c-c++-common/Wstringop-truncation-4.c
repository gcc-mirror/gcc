/* PR middle-end/84725 - enable attribute nonstring for all narrow character
   types
   Verify that -Wstringop-truncation is issued for uses of arrays and
   pointers to qualified forms of characters of all three types.
   { dg-do compile }
   { dg-options "-O2 -Wall -Wstringop-truncation -fno-ipa-icf" } */

#if __cplusplus
extern "C"
#endif
char* strncpy (char*, const char*, __SIZE_TYPE__);

#define S "1234"

struct Arrays
{
  char a[4];
  signed char b[4];
  unsigned char c[4];
};

void test_arrays (struct Arrays *p, const char *s)
{
  strncpy (p->a, s, sizeof p->a);           /* { dg-warning "\\\[-Wstringop-truncation" } */
  strncpy ((char*)p->b, s, sizeof p->b);    /* { dg-warning "\\\[-Wstringop-truncation" } */
  strncpy ((char*)p->c, s, sizeof p->c);    /* { dg-bogus "\\\[-Wstringop-truncation" } */
}

struct Pointers
{
  char *p;
  signed char *q;
  unsigned char *r;
};

void test_pointers (struct Pointers *p)
{
  strncpy (p->p, S, sizeof S - 1);          /* { dg-warning "\\\[-Wstringop-truncation" } */
  strncpy ((char*)p->q, S, sizeof S - 1);   /* { dg-warning "\\\[-Wstringop-truncation" } */
  strncpy ((char*)p->r, S, sizeof S - 1);   /* { dg-warning "\\\[-Wstringop-truncation" } */
}

struct ConstArrays
{
  const char a[4];
  const signed char b[4];
  const unsigned char c[4];
};

void test_const_arrays (struct ConstArrays *p, const char *s)
{
  strncpy ((char*)p->a, s, sizeof p->a);    /* { dg-warning "\\\[-Wstringop-truncation" } */
  strncpy ((char*)p->b, s, sizeof p->b);    /* { dg-warning "\\\[-Wstringop-truncation" } */
  strncpy ((char*)p->c, s, sizeof p->c);    /* { dg-bogus "\\\[-Wstringop-truncation" } */
}

struct ConstPointers
{
  const char *p;
  const signed char *q;
  const unsigned char *r;
};

void test_const_pointers (struct ConstPointers *p)
{
  strncpy ((char*)p->p, S, sizeof S - 1);   /* { dg-warning "\\\[-Wstringop-truncation" } */
  strncpy ((char*)p->q, S, sizeof S - 1);   /* { dg-warning "\\\[-Wstringop-truncation" } */
  strncpy ((char*)p->r, S, sizeof S - 1);   /* { dg-warning "\\\[-Wstringop-truncation" } */
}

struct VolatileArrays
{
  volatile char a[4];
  volatile signed char b[4];
  volatile unsigned char c[4];
};

void test_volatile_arrays (struct VolatileArrays *p, const char *s)
{
  strncpy ((char*)p->a, s, sizeof p->a);    /* { dg-warning "\\\[-Wstringop-truncation" } */
  strncpy ((char*)p->b, s, sizeof p->b);    /* { dg-warning "\\\[-Wstringop-truncation" } */
  strncpy ((char*)p->c, s, sizeof p->c);    /* { dg-bogus "\\\[-Wstringop-truncation" } */
}

struct VolatilePointers
{
  volatile char *p;
  volatile signed char *q;
  volatile unsigned char *r;
};

void test_volatile_pointers (struct VolatilePointers *p)
{
  strncpy ((char*)p->p, S, sizeof S - 1);   /* { dg-warning "\\\[-Wstringop-truncation" } */
  strncpy ((char*)p->q, S, sizeof S - 1);   /* { dg-warning "\\\[-Wstringop-truncation" } */
  strncpy ((char*)p->r, S, sizeof S - 1);   /* { dg-warning "\\\[-Wstringop-truncation" } */
}

struct ConstVolatileArrays
{
  const volatile char a[4];
  const volatile signed char b[4];
  const volatile unsigned char c[4];
};

void test_const_volatile_arrays (struct ConstVolatileArrays *p, const char *s)
{
  strncpy ((char*)p->a, s, sizeof p->a);    /* { dg-warning "\\\[-Wstringop-truncation" } */
  strncpy ((char*)p->b, s, sizeof p->b);    /* { dg-warning "\\\[-Wstringop-truncation" } */
  strncpy ((char*)p->c, s, sizeof p->c);    /* { dg-bogus "\\\[-Wstringop-truncation" } */
}

struct ConstVolatilePointers
{
  const volatile char *p;
  const volatile signed char *q;
  const volatile unsigned char *r;
};

void test_const_volatile_pointers (struct ConstVolatilePointers *p)
{
  strncpy ((char*)p->p, S, sizeof S - 1);   /* { dg-warning "\\\[-Wstringop-truncation" } */
  strncpy ((char*)p->q, S, sizeof S - 1);   /* { dg-warning "\\\[-Wstringop-truncation" } */
  strncpy ((char*)p->r, S, sizeof S - 1);   /* { dg-warning "\\\[-Wstringop-truncation" } */
}

/* { dg-prune-output "-Wdiscarded-qualifiers" } */
