/* PR middle-end/91490 - bogus argument missing terminating nul warning
   on strlen of a flexible array member
   Verify that strlen calls with a flexible array member (and its common
   forms) of declaraed objects are folded.
   { dg-do compile }
   { dg-options "-Wall -fdump-tree-gimple" } */

#include "strlenopt.h"

extern void* memchr (const void*, int, size_t);

struct A1 { char n, a[1]; };
struct A2 { char n, a[2]; };
struct A3 { char n, a[3]; };
struct Ax { char n, a[]; };

const struct A1 a1_0 = { 0 };
const struct A1 a1_0_ = { 0, { } };
const struct A1 a1_0_0 = { 0, { 0 } };

const struct A2 a2_1_ = { 1, { } };
const struct A2 a2_1_1 = { 1, { 1 } };
const struct A2 a2_1_1_0 = { 1, { 1, 0 } };

const struct A3 aa3_1_[2] = { { 1 } };

const struct Ax ax = { 3, { 3, 2, 1, 0 } };

struct BxA1 { int n; struct A1 a[]; };
struct BxA2 { int n; struct A2 a[]; };

const struct BxA2 bx = { 2, { { 2, { 2, 1 } }, { 2, { 1, 0 } } } };

#if 0

// Not implemented yet.

int mchr1, mchr1__, mchr1_0, mchr2_1, mchr2, mchr2_1_0, mchrax, mchrbx;

void test_memchr_flexarray (void)
{
  mchr1 = 0 != memchr (&a1_0, '1', sizeof a1_0);
  mchr1__ = 0 != memchr (&a1_0_, '2', sizeof a1_0_);
  mchr1_0 = 0 != memchr (&a1_0_0, '3', sizeof a1_0_0);

  mchr2 = 0 != memchr (&a2_1_, '4', sizeof a2_1_);
  mchr2_1 = 0 != memchr (&a2_1_1, '\001', sizeof a2_1_1);
  mchr2_1_0 = 0 != memchr (&a2_1_1_0, '\001', sizeof a2_1_1_0);

  mchrax = (const char*)&ax + sizeof ax - 1 == memchr (&ax, '\001', sizeof ax);
  mchrbx = (const char*)&bx + sizeof bx - 1 == memchr (&bx, '\001', sizeof bx);
}

#endif


int schr1, schr1__, schr1_0, schr2_1, schr2, schr2_1_0, schrax, schrbx;

void test_strchr_flexarray (void)
{
  schr1 = 0 != strchr (a1_0.a, '1');
  schr1__ = 0 != strchr (a1_0_.a, '2');
  schr1_0 = 0 != strchr (a1_0_0.a, '3');

  schr2 = 0 != strchr (a2_1_.a, '4');
  schr2_1 = 0 != strchr (a2_1_1.a, '\001');
  schr2_1_0 = 0 != strchr (a2_1_1_0.a, '\001');

  schrax = 0 != strchr (ax.a, '\001');
  schrbx = 0 != strchr (bx.a[1].a, '\0');

  /* { dg-final { scan-tree-dump "schr1 = 0;" "gimple" } }
     { dg-final { scan-tree-dump "schr1__ = 0;" "gimple" } }
     { dg-final { scan-tree-dump "schr1_0 = 0;" "gimple" } }
     { dg-final { scan-tree-dump "schr2 = 0;" "gimple" } }
     { dg-final { scan-tree-dump "schr2_1 = 1;" "gimple" } }
     { dg-final { scan-tree-dump "schr2_1_0 = 1;" "gimple" } }
     { dg-final { scan-tree-dump "schrax = 1;" "gimple" } }
     { dg-final { scan-tree-dump "schrbx = 1;" "gimple" } } */
}


int scmp1, scmp1__, scmp1_0, scmp2_1, scmp2, scmp2_1_0, scmpax, scmpbx;

void test_strcmp_flexarray (void)
{
  scmp1 = 0 == strcmp (a1_0.a, "1");
  scmp1__ = 0 == strcmp (a1_0_.a, "2");
  scmp1_0 = 0 == strcmp (a1_0_0.a, "3");

  scmp2 = 0 == strcmp (a2_1_.a, "4");
  scmp2_1 = 0 == strcmp (a2_1_1.a, "\001");
  scmp2_1_0 = 0 == strcmp (a2_1_1_0.a, "\001");

  scmpax = 0 == strcmp (ax.a, "\003\002\001");
  scmpbx = 0 == strcmp (bx.a[1].a, "\001");

  /* { dg-final { scan-tree-dump "scmp1 = 0;" "gimple" } }
     { dg-final { scan-tree-dump "scmp1__ = 0;" "gimple" } }
     { dg-final { scan-tree-dump "scmp1_0 = 0;" "gimple" } }
     { dg-final { scan-tree-dump "scmp2 = 0;" "gimple" } }
     { dg-final { scan-tree-dump "scmp2_1 = 1;" "gimple" } }
     { dg-final { scan-tree-dump "scmp2_1_0 = 1;" "gimple" } }
     { dg-final { scan-tree-dump "scmpax = 1;" "gimple" } }
     { dg-final { scan-tree-dump "scmpbx = 1;" "gimple" } } */
}


int len1, len1__, len1_0, len2_1, len2, len2_1_0, lenax, lenbx;

void test_strlen_flexarray (void)
{
  len1 = strlen (a1_0.a);
  len1__ = strlen (a1_0_.a);
  len1_0 = strlen (a1_0_0.a);

  len2 = strlen (a2_1_.a);
  len2_1 = strlen (a2_1_1.a);
  len2_1_0 = strlen (a2_1_1_0.a);

  lenax = strlen (ax.a);
  lenbx = strlen (bx.a[1].a);

  /* { dg-final { scan-tree-dump "len1 = 0;" "gimple" } }
     { dg-final { scan-tree-dump "len1__ = 0;" "gimple" } }
     { dg-final { scan-tree-dump "len1_0 = 0;" "gimple" } }
     { dg-final { scan-tree-dump "len2 = 0;" "gimple" } }
     { dg-final { scan-tree-dump "len2_1 = 1;" "gimple" } }
     { dg-final { scan-tree-dump "len2_1_0 = 1;" "gimple" } }
     { dg-final { scan-tree-dump "lenax = 3;" "gimple" } }
     { dg-final { scan-tree-dump "lenbx = 1;" "gimple" } } */
}


int schraa3, scmpaa3, lenaa3;

void test_trailing_array_empty_init (void)
{
  schraa3 = ((aa3_1_[0].a == strchr (aa3_1_[0].a, 0))
	     + (aa3_1_[1].a == strchr (aa3_1_[1].a, 0)));

  scmpaa3 = strcmp (aa3_1_[0].a, aa3_1_[1].a);
  lenaa3 = strlen (aa3_1_[0].a) + strlen (aa3_1_[1].a);

  /* { dg-final { scan-tree-dump "schraa3 = 2;" "gimple" } }
     { dg-final { scan-tree-dump "scmpaa3 = 0;" "gimple" } }
     { dg-final { scan-tree-dump "lenaa3 = 0;" "gimple" } }  */
}

union U4 { char a[4]; int i; };
const union U4 u4[2] = { { "123" } };

int ulen0, ulen1;

void test_union_init (void)
{
  ulen0 = strlen (u4[0].a);
  ulen1 = strlen (u4[1].a);

  /* { dg-final { scan-tree-dump "ulen0 = 3;" "gimple" } }
     { dg-final { scan-tree-dump "ulen1 = 0;" "gimple" } } */
}

/* { dg-final { scan-tree-dump-not "strchr *\\(" "gimple" } }
   { dg-final { scan-tree-dump-not "strcmp *\\(" "gimple" } }
   { dg-final { scan-tree-dump-not "strlen *\\(" "gimple" } } */
