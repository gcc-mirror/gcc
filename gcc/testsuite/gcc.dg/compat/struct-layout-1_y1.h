#include "struct-layout-1.h"

#define F(n, x, v, w) 						\
  if (info.flds[i] != &s##n.x)					\
    FAIL (n, 50);						\
  if (info.sizes[i] != sizeof (s##n.x))				\
    FAIL (n, 51);						\
  if (info.aligns[i] != __alignof__ (s##n.x))			\
    FAIL (n, 52);						\
  if (s##n.x != (__typeof__ (s##n.x)) v)			\
    FAIL (n, 53);						\
  if (a##n[2].x != (__typeof__ (s##n.x)) w)			\
    FAIL (n, 54);						\
  if (arg0.x != s##n.x)						\
    FAIL (n, 55);						\
  if (arg2.x != a##n[2].x)					\
    FAIL (n, 56);						\
  ret.x = s##n.x;						\
  ++i;
#define N(n, x) 						\
  if (info.flds[i] != &s##n.x)					\
    FAIL (n, 50);						\
  if (info.sizes[i] != sizeof (s##n.x))				\
    FAIL (n, 51);						\
  if (info.aligns[i] != __alignof__ (s##n.x))			\
    FAIL (n, 52);						\
  ++i;
#define B(n, x, v, w) 						\
  b1.x = v; b2.x = w;						\
  if (s##n.x != b1.x)						\
    FAIL (n, 53);						\
  if (a##n[2].x != b2.x)					\
    FAIL (n, 54);						\
  if (arg0.x != s##n.x)						\
    FAIL (n, 55);						\
  if (arg2.x != a##n[2].x)					\
    FAIL (n, 56);						\
  ret.x = s##n.x;						\
  ++j;
#define TX(n, type, attrs, fields, ops) 			\
type S##n { fields } attrs;					\
extern type S##n s##n;						\
type S##n a##n[5];						\
type S##n							\
check##n (type S##n arg0, type S##n *arg1, type S##n arg2)	\
{								\
  type S##n ret;						\
  type S##n b1, b2;						\
  int i, j;							\
								\
  memset (&ret, 0, sizeof (ret));				\
  memset (&b1, 0, sizeof (b1));					\
  memset (&b2, 0, sizeof (b2));					\
  if (info.sp != &s##n)						\
    FAIL (n, 10);						\
  if (info.a0p != &a##n[0])					\
    FAIL (n, 11);						\
  if (info.a3p != &a##n[3])					\
    FAIL (n, 12);						\
  if (info.sz != sizeof (s##n))					\
    FAIL (n, 13);						\
  if (info.als != __alignof__ (s##n))				\
    FAIL (n, 14);						\
  if (info.ala0 != __alignof__ (a##n[0]))			\
    FAIL (n, 15);						\
  if (info.ala3 != __alignof__ (a##n[3]))			\
    FAIL (n, 16);						\
  if (arg1 != &a##n[1])						\
    FAIL (n, 17);						\
  i = 0; j = 0;							\
  ops								\
  if (i != info.nfields || j != info.nbitfields)		\
    FAIL (n, 18);						\
								\
  return ret;							\
}
