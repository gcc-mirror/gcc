/* PR 85643 - attribute nonstring fails to squash -Wstringop-truncation
   warning
  { dg-do compile }
  { dg-options "-O2 -Wall -ftrack-macro-expansion=0" } */

#define strncpy   __builtin_strncpy

struct A {
  char a[2][16 + 1];
};

struct B {
  char a[2][16] __attribute__ ((__nonstring__));
};

struct B*
test_memarray (const struct A *s)
{
  static struct B b;
  strncpy (b.a[1], s->a[1], sizeof b.a[1]);
  return &b;
}

const char*
test_array (const char *s)
{
  static char a[2][80] __attribute__ ((__nonstring__));
  strncpy (a[1], s, sizeof a[1]);
  return a[1];
}

const char*
test_array_idx (const char *s)
{
  static char a[2][80]  __attribute__ ((__nonstring__));
  char *p __attribute__ ((__nonstring__)) = &a[1][20];
  strncpy (p, s, 60);   /* { dg-bogus "-Wstringop-truncation" } */
  return a[1];
}

const char*
test_array_off (const char *s)
{
  static char a[2][80]  __attribute__ ((__nonstring__));
  char *p __attribute__ ((__nonstring__)) = a[1] + 20;
  strncpy (p, s, 60);   /* { dg-bogus "-Wstringop-truncation" } */
  return a[1];
}

struct B*
test_memarray_cstidx_idx (const char *s)
{
  static struct B b[2];
  char *p __attribute__ ((__nonstring__)) = &b[1].a[1][4];

  /* The destination below is represented as &MEM[(void *)&a + 20B] and
     which (in general) doesn't make it possible to determine what member
     it refers to.  */
  strncpy (p, s, sizeof b[1].a[1] - 4);   /* { dg-bogus "-Wstringop-truncation" } */
  return b;
}

struct B*
test_memarray_cstidx_off (const char *s)
{
  static struct B b[2];
  char *p __attribute__ ((__nonstring__)) = b[1].a[1] + 4;

  /* Same as above.  */
  strncpy (p, s, sizeof b[1].a[1] - 4);   /* { dg-bogus "-Wstringop-truncation" "" { xfail *-*-*} } */
  return b;
}

struct B*
test_memarray_varidx_idx (const char *s, int i)
{
  static struct B b[3];
  char *p __attribute__ ((__nonstring__)) = &b[i].a[1][4];
  strncpy (p, s, sizeof b[i].a[1] - 4);
  return b;
}

struct B*
test_memarray_varidx_off (const char *s, int i)
{
  static struct B b[3];
  char *p __attribute__ ((__nonstring__)) = b[i].a[1] + 4;
  strncpy (p, s, sizeof b[i].a[1] - 4);
  return b;
}
