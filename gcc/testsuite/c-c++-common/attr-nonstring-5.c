/* PR middle-end/84725 - enable attribute nonstring for all narrow character
   types
   Verify that using attribute nonstring with all three narrow character
   types is accepted and using arrays and pointers to characters of all
   three types (including their qualified forms) declared with the
   attributes doesn't trigger -Wstringop-truncation warnings.
   { dg-do compile }
   { dg-options "-O -Wall -Wstringop-truncation" } */

#if __cplusplus
extern "C"
#endif
char* strncpy (char*, const char*, __SIZE_TYPE__);

#define NONSTR __attribute__ ((nonstring))

#define S "1234"

struct Arrays
{
  char NONSTR a[4];
  signed char NONSTR b[4];
  unsigned char NONSTR c[4];
};

void test_arrays (struct Arrays *p, const char *s)
{
  strncpy (p->a, s, sizeof p->a);
  strncpy ((char*)p->b, s, sizeof p->b);
  strncpy ((char*)p->c, s, sizeof p->c);
}

struct Pointers
{
  char NONSTR *p;
  signed char NONSTR *q;
  unsigned char NONSTR *r;
};

void test_pointers (struct Pointers *p)
{
  strncpy (p->p, S, sizeof S - 1);
  strncpy ((char*)p->q, S, sizeof S - 1);
  strncpy ((char*)p->r, S, sizeof S - 1);
}

struct ConstArrays
{
  const char NONSTR a[4];
  const signed char NONSTR b[4];
  const unsigned char NONSTR c[4];
};

void test_const_arrays (struct ConstArrays *p, const char *s)
{
  strncpy ((char*)p->a, s, sizeof p->a);
  strncpy ((char*)p->b, s, sizeof p->b);
  strncpy ((char*)p->c, s, sizeof p->c);
}

struct ConstPointers
{
  const char NONSTR *p;
  const signed char NONSTR *q;
  const unsigned char NONSTR *r;
};

void test_const_pointers (struct ConstPointers *p)
{
  strncpy ((char*)p->p, S, sizeof S - 1);
  strncpy ((char*)p->q, S, sizeof S - 1);
  strncpy ((char*)p->r, S, sizeof S - 1);
}

struct VolatileArrays
{
  volatile char NONSTR a[4];
  volatile signed char NONSTR b[4];
  volatile unsigned char NONSTR c[4];
};

void test_volatile_arrays (struct VolatileArrays *p, const char *s)
{
  strncpy ((char*)p->a, s, sizeof p->a);
  strncpy ((char*)p->b, s, sizeof p->b);
  strncpy ((char*)p->c, s, sizeof p->c);
}

struct VolatilePointers
{
  volatile char NONSTR *p;
  volatile signed char NONSTR *q;
  volatile unsigned char NONSTR *r;
};

void test_volatile_pointers (struct VolatilePointers *p)
{
  strncpy ((char*)p->p, S, sizeof S - 1);
  strncpy ((char*)p->q, S, sizeof S - 1);
  strncpy ((char*)p->r, S, sizeof S - 1);
}

struct ConstVolatileArrays
{
  const volatile char NONSTR a[4];
  const volatile signed char NONSTR b[4];
  const volatile unsigned char NONSTR c[4];
};

void test_const_volatile_arrays (struct ConstVolatileArrays *p, const char *s)
{
  strncpy ((char*)p->a, s, sizeof p->a);
  strncpy ((char*)p->b, s, sizeof p->b);
  strncpy ((char*)p->c, s, sizeof p->c);
}

struct ConstVolatilePointers
{
  const volatile char NONSTR *p;
  const volatile signed char NONSTR *q;
  const volatile unsigned char NONSTR *r;
};

void test_const_volatile_pointers (struct ConstVolatilePointers *p)
{
  strncpy ((char*)p->p, S, sizeof S - 1);
  strncpy ((char*)p->q, S, sizeof S - 1);
  strncpy ((char*)p->r, S, sizeof S - 1);
}

/* { dg-prune-output "-Wdiscarded-qualifiers" } */
