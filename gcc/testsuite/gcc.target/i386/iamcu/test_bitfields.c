/* This is a small test to see if bitfields are working.  It is only a
   few structs and a union and a test to see if they have the correct
   size, if values can be read and written and a couple of argument
   passing tests.  No alignment testing is done.  */

#include "defines.h"
#include "macros.h"


/* These five bitfields are taken from the System V ABI, Intel 386
   architecture supplement.  */

/* Word aligned, sizeof is 4.  */
struct RightToLeft
{
  int j:5;
  int k:6;
  int m:7;
};

/* Word aligned, sizeof is 12.  */
struct BoundaryAlignment
{
  short s:9;
  int   j:9;
  char  c;
  short t:9;
  short u:9;
  char  d;
};

/* Halfword aligned, sizeof is 2.  */
struct StorageUnitSharing
{
  char  c;
  short s:8;
};

/* Halfword aligned, sizeof is 2.  */
union Allocation
{
  char  c;
  short s:8;
};

/* Byte aligned, sizeof is 9.  */
struct Unnamed
{
  char  c;
  int    :0;
  char  d;
  short  :9;
  char  e;
  char   :0;
};

/* Extra struct testing bitfields in larger types.
   Doubleword aligned, sizeof is 8.  */
struct LargerTypes
{
  long long l:33;
  int       i:31;
};


void
passing1 (struct RightToLeft str, int j, int k, int m)
{
  assert (str.j == j);
  assert (str.k == k);
  assert (str.m == m);
}

void
passing2 (struct BoundaryAlignment str, short s, int j, char c, short t,
	  short u, char d)
{
  assert (str.s == s);
  assert (str.j == j);
  assert (str.c == c);
  assert (str.t == t);
  assert (str.u == u);
  assert (str.d == d);
}

void
passing3 (struct StorageUnitSharing str, char c, short s)
{
  assert (str.c == c);
  assert (str.s == s);
}

void
passing4 (struct Unnamed str, char c, char d, char e)
{
  assert (str.c == c);
  assert (str.d == d);
  assert (str.e == e);
}

void
passing5 (struct LargerTypes str, long long l, int i)
{
  assert (str.l == l);
  assert (str.i == i);
}


void
passingU (union Allocation u, char c)
{
  assert (u.c == c);
  assert (u.s == c);
}


int
main (void)
{
  struct RightToLeft str1;
  struct BoundaryAlignment str2;
  struct StorageUnitSharing str3;
  struct Unnamed str4;
  struct LargerTypes str5;
  union Allocation u;

  /* Check sizeof's.  */
  check_size(str1, 4);
  check_size(str2, 12);
  check_size(str3, 2);
  check_size(str4, 9);
  check_size(str5, 8);
  check_size(u, 2);

  /* Check alignof's.  */
  check_align_lv(str1, 4);
  check_align_lv(str2, 4);
  check_align_lv(str3, 2);
  check_align_lv(str4, 1);
  check_align_lv(str5, 4);
  check_align_lv(u, 2);

  /* Check passing.  */
  str1.j = str2.s = str3.c = str4.c = str5.l = 4;
  str1.k = str2.j = str3.s = str4.d = str5.i = 5;
  str1.m = str2.c = str4.e = 6;
  str2.t = 7;
  str2.u = 8;
  str2.d = 9;
  passing1 (str1, 4, 5, 6);
  passing2 (str2, 4, 5, 6, 7, 8, 9);
  passing3 (str3, 4, 5);
  passing4 (str4, 4, 5, 6);
  passing5 (str5, 4, 5);

  u.c = 5;
  passingU (u, 5);
  u.s = 6;
  passingU (u, 6);

  return 0;
}
