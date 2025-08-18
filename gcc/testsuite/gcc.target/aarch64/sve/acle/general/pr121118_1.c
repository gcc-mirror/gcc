/* { dg-options "-O2 -msve-vector-bits=512" } */

typedef __SVBool_t fixed_bool __attribute__((arm_sve_vector_bits(512)));

#define TEST_CONST(NAME, CONST)						\
  fixed_bool								\
  NAME ()								\
  {									\
    union { unsigned long long i; fixed_bool pg; } u = { CONST };	\
    return u.pg;							\
  }

TEST_CONST (test1, 0x02aaaaaaaa)
TEST_CONST (test2, 0x0155555557)
TEST_CONST (test3, 0x0013333333333333ULL)
TEST_CONST (test4, 0x0011111111111113ULL)
