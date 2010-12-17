/* Test semantics of #pragma pack.
   Contributed by Mike Coleman <mcoleman2@kc.rr.com> */

/* { dg-do compile { target { ! default_packed } } } */

/* Mainly we're just testing whether pushing and popping seem to be
   working correctly, and verifying the (alignment == 1) case, which
   is really the only reason anyone would use this pragma anyway. */

#include <stddef.h>

/* generalized compile-time test expression */
#define test(n, expr) int test_##n [(expr) ? 1 : -1]

/* Round V down to multiple of A */
#define floor(v,a) ((v) / (a) * (a))

/* Offset of field with alignment A in structure S after a field P of
   type PT */
#define offset(s,p,pt,a) \
	floor ((offsetof(struct s, p) + sizeof (pt) + (a) - 1), a)

/* regular minimum */
#define min(a,b)  ((a) < (b) ? (a) : (b))

/* Check that field A (type AT) followed by field B (type BT) are
   packed according to P */
#define test_pack(n, a, at, b, bt, p) \
	test(n, offsetof (struct SNAME, b) \
 	        == min (offset (SNAME,a,at,__alignof__(bt)), \
		        offset (SNAME,a,at,p)))

/* Test offset of field F in structs s1 and s2 are the same.  */
#define test_offset(n, s1, s2, f) \
	test (n, (offsetof(struct s1, f) == offsetof(struct s2, f)))

#define SNAME s0
#include "pack-test-1.h"

#undef SNAME
#define SNAME s1
#pragma pack(push, p1, 1)
#include "pack-test-1.h"

void SNAME() {
  test_pack(0, f0, char, f1, double, 1);
  test_pack(1, f2, short, f3, double, 1);
  test_pack(2, f4, int, f5, double, 1);
}

#undef SNAME
#define SNAME s2
#pragma pack(push, p2, 2)
#include "pack-test-1.h"

void SNAME() {
  test_pack(0, f0, char, f1, double, 2);
  test_pack(1, f2, short, f3, double, 2);
  test_pack(2, f4, int, f5, double, 2);
}

#undef SNAME
#define SNAME s3
#pragma pack(push, p3, 4)
#include "pack-test-1.h"

void SNAME() {
  test_pack(0, f0, char, f1, double, 4);
  test_pack(1, f2, short, f3, double, 4);
  test_pack(2, f4, int, f5, double, 4);
}

#undef SNAME
#define SNAME s4
#pragma pack(pop)
#include "pack-test-1.h"

void SNAME() {
  test_pack(0, f0, char, f1, double, 2);
  test_pack(1, f2, short, f3, double, 2);
  test_pack(2, f4, int, f5, double, 2);
}

#undef SNAME
#define SNAME s5
#pragma pack(pop, p2)
#include "pack-test-1.h"

void SNAME() {
  test_pack(0, f0, char, f1, double, 1);
  test_pack(1, f2, short, f3, double, 1);
  test_pack(2, f4, int, f5, double, 1);
}

#undef SNAME
#define SNAME s6
#pragma pack(pop, p1)
#include "pack-test-1.h"

void SNAME() {
  test_offset (0, s0, SNAME, f0);
  test_offset (1, s0, SNAME, f1);
  test_offset (2, s0, SNAME, f2);
  test_offset (3, s0, SNAME, f3);
  test_offset (4, s0, SNAME, f4);
  test_offset (5, s0, SNAME, f5);
}

#undef SNAME
#define SNAME s7
#pragma pack(1)
#include "pack-test-1.h"

void SNAME() {
  test_pack(0, f0, char, f1, double, 1);
  test_pack(1, f2, short, f3, double, 1);
  test_pack(2, f4, int, f5, double, 1);
}

#undef SNAME
#define SNAME s8
#pragma pack(push, p2, 2)
#include "pack-test-1.h"

void SNAME() {
  test_pack(0, f0, char, f1, double, 2);
  test_pack(1, f2, short, f3, double, 2);
  test_pack(2, f4, int, f5, double, 2);
}

#undef SNAME
#define SNAME s9
#pragma pack(pop)
#include "pack-test-1.h"

void SNAME() {
  test_pack(0, f0, char, f1, double, 1);
  test_pack(1, f2, short, f3, double, 1);
  test_pack(2, f4, int, f5, double, 1);
}

#undef SNAME
#define SNAME s10
#pragma pack()
#include "pack-test-1.h"

void SNAME() {
  test_offset (0, s0, SNAME, f0);
  test_offset (1, s0, SNAME, f1);
  test_offset (2, s0, SNAME, f2);
  test_offset (3, s0, SNAME, f3);
  test_offset (4, s0, SNAME, f4);
  test_offset (5, s0, SNAME, f5);
}
