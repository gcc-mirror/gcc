#include "struct-layout-1.h"

struct Info info;
int fails;
int intarray[256];
int fn0 (void) { return 0; }
int fn1 (void) { return 1; }
int fn2 (void) { return 2; }
int fn3 (void) { return 3; }
int fn4 (void) { return 4; }
int fn5 (void) { return 5; }
int fn6 (void) { return 6; }
int fn7 (void) { return 7; }
int fn8 (void) { return 8; }
int fn9 (void) { return 9; }

/* This macro is intended for fields where their
   addresses/sizes/alignments and value passing should be checked.  */
#define F(n, x, v, w) 						\
  info.flds[i] = &s##n.x;					\
  info.sizes[i] = sizeof (s##n.x);				\
  info.aligns[i] = __alignof__ (s##n.x);			\
  s##n.x = v;							\
  a##n[2].x = w;						\
  ++i;
/* This macro is for fields where just their addresses/sizes/alignments
   should be checked.  */
#define N(n, x) 						\
  info.flds[i] = &s##n.x;					\
  info.sizes[i] = sizeof (s##n.x);				\
  info.aligns[i] = __alignof__ (s##n.x);			\
  ++i;
/* This macro is for fields where just value passing should be checked.  */
#define B(n, x, v, w)						\
  s##n.x = v;							\
  a##n[2].x = w;						\
  ++j;
#define TX(n, type, attrs, fields, ops) 			\
type S##n { fields } attrs;					\
type S##n s##n;							\
extern type S##n a##n[5];					\
extern type S##n check##n (type S##n, type S##n *,		\
			     type S##n);			\
extern void check##n##va (int i, ...);				\
extern void checkx##n (type S##n);				\
void test##n (void)						\
{								\
  int i, j;							\
  memset (&s##n, '\0', sizeof (s##n));				\
  memset (a##n, '\0', sizeof (a##n));				\
  memset (&info, '\0', sizeof (info));				\
  info.sp = &s##n;						\
  info.a0p = &a##n[0];						\
  info.a3p = &a##n[3];						\
  info.sz = sizeof (s##n);					\
  info.als = __alignof__ (s##n);				\
  info.ala0 = __alignof__ (a##n[0]);				\
  info.ala3 = __alignof__ (a##n[3]);				\
  if (((long) (__SIZE_TYPE__) &a##n[3]) & (info.als - 1))			\
    FAIL (n, 1);						\
  i = 0; j = 0;							\
  ops								\
  info.nfields = i;						\
  info.nbitfields = j;						\
  checkx##n (check##n (s##n, &a##n[1], a##n[2]));		\
  check##n##va (1, 1.0, s##n, 2LL, a##n[2], a##n[2]);		\
  check##n##va (2, s##n, s##n, 2.0L, a##n[2], s##n);		\
}
