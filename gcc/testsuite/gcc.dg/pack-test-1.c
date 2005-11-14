/* Test semantics of #pragma pack.
   Contributed by Mike Coleman <mcoleman2@kc.rr.com> */

/* { dg-do compile { target { { *-*-linux* *-*-cygwin* powerpc*-*-eabi* } && { ! default_packed } } } } */

/* We only test the alignment of char, short, and int, because these
   are the only ones that are pretty certain to be the same across
   platforms (and maybe not even those).  Mainly we're just testing
   whether pushing and popping seem to be working correctly, and
   verifying the (alignment == 1) case, which is really the only
   reason anyone would use this pragma anyway.
*/

#include <stddef.h>

/* gap in bytes between fields a and b in struct s */
#define gap(s, a, b) (offsetof(struct s, a) - offsetof(struct s, b))
/* generalized compile-time test expression */
#define test(n, expr) int test_##n [(expr) ? 1 : -1]
/* test a gap */
#define testgap(n, a, b, val) test(n, gap(SNAME, a, b) == val)

#define SNAME s0
#include "pack-test-1.h"

/* Save original alignment values.  Can't use const ints because they
   won't be expanded and we'll get bogus errors about variable length
   arrays.  (Possible bug in C front end?)  Use s0, not SNAME, so these
   won't change later.  */
#define al1 gap(s0, f1, f0)
#define al2 gap(s0, f2, f1)
#define al3 gap(s0, f3, f2)
#define al4 gap(s0, f4, f3)
#define al5 gap(s0, f5, f4)
#define al6 gap(s0, f6, f5)
#define al7 gap(s0, f7, f6)

#undef SNAME
#define SNAME s1
#pragma pack(push, p1, 1)
#include "pack-test-1.h"

void SNAME() {
  testgap(0, f1, f0, sizeof(char));
  testgap(1, f3, f2, sizeof(short));
  testgap(2, f5, f4, sizeof(int));
}

#undef SNAME
#define SNAME s2
#pragma pack(push, p2, 2)
#include "pack-test-1.h"

void SNAME() {
  testgap(0, f1, f0, sizeof(short));
  testgap(1, f3, f2, sizeof(short));
  testgap(2, f5, f4, sizeof(int));
}

#undef SNAME
#define SNAME s3
#pragma pack(push, p3, 4)
#include "pack-test-1.h"

void SNAME() {
  testgap(0, f1, f0, sizeof(int));
  testgap(1, f3, f2, sizeof(int));
  testgap(2, f5, f4, sizeof(int));
}

#undef SNAME
#define SNAME s4
#pragma pack(pop)
#include "pack-test-1.h"

void SNAME() {
  testgap(0, f1, f0, sizeof(short));
  testgap(1, f3, f2, sizeof(short));
  testgap(2, f5, f4, sizeof(int));
}

#undef SNAME
#define SNAME s5
#pragma pack(pop, p2)
#include "pack-test-1.h"

void SNAME() {
  testgap(0, f1, f0, sizeof(char));
  testgap(1, f3, f2, sizeof(short));
  testgap(2, f5, f4, sizeof(int));
}

#undef SNAME
#define SNAME s6
#pragma pack(pop, p1)
#include "pack-test-1.h"

void SNAME() {
  testgap(0, f1, f0, al1);
  testgap(1, f3, f2, al3);
  testgap(2, f5, f4, al5);
}

#undef SNAME
#define SNAME s7
#pragma pack(1)
#include "pack-test-1.h"

void SNAME() {
  testgap(0, f1, f0, sizeof(char));
  testgap(1, f3, f2, sizeof(short));
  testgap(2, f5, f4, sizeof(int));
}

#undef SNAME
#define SNAME s8
#pragma pack(push, p2, 2)
#include "pack-test-1.h"

void SNAME() {
  testgap(0, f1, f0, sizeof(short));
  testgap(1, f3, f2, sizeof(short));
  testgap(2, f5, f4, sizeof(int));
}

#undef SNAME
#define SNAME s9
#pragma pack(pop)
#include "pack-test-1.h"

void SNAME() {
  testgap(0, f1, f0, sizeof(char));
  testgap(1, f3, f2, sizeof(short));
  testgap(2, f5, f4, sizeof(int));
}

#undef SNAME
#define SNAME s10
#pragma pack()
#include "pack-test-1.h"

void SNAME() {
  testgap(0, f1, f0, al1);
  testgap(1, f3, f2, al3);
  testgap(2, f5, f4, al5);
}
