/* Test the rcpc3 ACLE intrinsics.  */
/* { dg-do compile } */
/* { dg-options "-O2 -march=armv8.2-a+rcpc3" } */
#include <stdint.h>
#include <arm_neon.h>

#define TEST_LDAP(TYPE, T)						\
  TYPE##x##1_t T##1_test (TYPE##_t const * ptr, TYPE##x##1_t src) {	\
    return vldap1_lane_##T##64 (ptr, src, 0);				\
  }

#define TEST_LDAPQ(TYPE, T)						\
  TYPE##x##2_t T##2_test (TYPE##_t const * ptr, TYPE##x##2_t src) {	\
    return vldap1q_lane_##T##64 (ptr, src, 1);				\
  }

#define TEST_STL(TYPE, T)				    \
  void T##1s_test (TYPE##_t * ptr, TYPE##x##1_t src) {	    \
    vstl1_lane_##T##64 (ptr, src, 0);			    \
  }

#define TEST_STLQ(TYPE, T)		    \
  void T##2s_test (TYPE##_t * ptr, TYPE##x##2_t src) {	    \
    vstl1q_lane_##T##64 (ptr, src, 1);			    \
  }

TEST_LDAP (uint64, u);
TEST_LDAP (int64, s);
TEST_LDAP (float64, f);
TEST_LDAP (poly64, p);
/* { dg-final { scan-assembler-times {ldap1\t\{v\d.d\}\[0\], \[x\d\]} 4 } } */
TEST_LDAPQ (uint64, u);
TEST_LDAPQ (int64, s);
TEST_LDAPQ (float64, f);
TEST_LDAPQ (poly64, p);
/* { dg-final { scan-assembler-times {ldap1\t\{v\d.d\}\[1\], \[x\d\]} 4 } } */

TEST_STL (uint64, u);
TEST_STL (int64, s);
TEST_STL (float64, f);
TEST_STL (poly64, p);
/* { dg-final { scan-assembler-times {stl1\t\{v\d.d\}\[0\], \[x\d\]} 4 } } */
TEST_STLQ (uint64, u);
TEST_STLQ (int64, s);
TEST_STLQ (float64, f);
TEST_STLQ (poly64, p);
/* { dg-final { scan-assembler-times {stl1\t\{v\d.d\}\[1\], \[x\d\]} 4 } } */
