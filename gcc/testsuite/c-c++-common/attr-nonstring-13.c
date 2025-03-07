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
  char NONSTR a[4][4];
  signed char NONSTR b[4][4];
  unsigned char NONSTR c[4][4];
};

void test_arrays (struct Arrays *p, const char *s)
{
  strncpy (p->a[2], s, sizeof p->a[2]);
  strncpy ((char*)p->b[2], s, sizeof p->b[2]);
  strncpy ((char*)p->c[2], s, sizeof p->c[2]);
}

struct Pointers
{
  char NONSTR (*p)[4];
  signed char NONSTR (*q)[4];
  unsigned char NONSTR (*r)[4];
};

void test_pointers (struct Pointers *p)
{
  strncpy (*p->p, S, sizeof S - 1);
  strncpy ((char*)*p->q, S, sizeof S - 1);
  strncpy ((char*)*p->r, S, sizeof S - 1);
}

struct ConstArrays
{
  const char NONSTR a[4][4];
  const signed char NONSTR b[4][4];
  const unsigned char NONSTR c[4][4];
};

void test_const_arrays (struct ConstArrays *p, const char *s)
{
  strncpy ((char*)p->a[2], s, sizeof p->a[2]);
  strncpy ((char*)p->b[2], s, sizeof p->b[2]);
  strncpy ((char*)p->c[2], s, sizeof p->c[2]);
}

struct ConstPointers
{
  const char NONSTR (*p)[4];
  const signed char NONSTR (*q)[4];
  const unsigned char NONSTR (*r)[4];
};

void test_const_pointers (struct ConstPointers *p)
{
  strncpy ((char*)*p->p, S, sizeof S - 1);
  strncpy ((char*)*p->q, S, sizeof S - 1);
  strncpy ((char*)*p->r, S, sizeof S - 1);
}

struct VolatileArrays
{
  volatile char NONSTR a[4][4];
  volatile signed char NONSTR b[4][4];
  volatile unsigned char NONSTR c[4][4];
};

void test_volatile_arrays (struct VolatileArrays *p, const char *s)
{
  strncpy ((char*)p->a[2], s, sizeof p->a[2]);
  strncpy ((char*)p->b[2], s, sizeof p->b[2]);
  strncpy ((char*)p->c[2], s, sizeof p->c[2]);
}

struct VolatilePointers
{
  volatile char NONSTR (*p)[4];
  volatile signed char NONSTR (*q)[4];
  volatile unsigned char NONSTR (*r)[4];
};

void test_volatile_pointers (struct VolatilePointers *p)
{
  strncpy ((char*)*p->p, S, sizeof S - 1);
  strncpy ((char*)*p->q, S, sizeof S - 1);
  strncpy ((char*)*p->r, S, sizeof S - 1);
}

struct ConstVolatileArrays
{
  const volatile char NONSTR a[4][4];
  const volatile signed char NONSTR b[4][4];
  const volatile unsigned char NONSTR c[4][4];
};

void test_const_volatile_arrays (struct ConstVolatileArrays *p, const char *s)
{
  strncpy ((char*)p->a[2], s, sizeof p->a[2]);
  strncpy ((char*)p->b[2], s, sizeof p->b[2]);
  strncpy ((char*)p->c[2], s, sizeof p->c[2]);
}

struct ConstVolatilePointers
{
  const volatile char NONSTR (*p)[4];
  const volatile signed char NONSTR (*q)[4];
  const volatile unsigned char NONSTR (*r)[4];
};

void test_const_volatile_pointers (struct ConstVolatilePointers *p)
{
  strncpy ((char*)*p->p, S, sizeof S - 1);
  strncpy ((char*)*p->q, S, sizeof S - 1);
  strncpy ((char*)*p->r, S, sizeof S - 1);
}

/* { dg-prune-output "-Wdiscarded-qualifiers" } */
