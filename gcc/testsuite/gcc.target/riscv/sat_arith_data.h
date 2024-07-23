#ifndef HAVE_DEFINED_SAT_ARITH_DATA_H
#define HAVE_DEFINED_SAT_ARITH_DATA_H

#define TEST_UNARY_STRUCT_NAME(T1, T2) test_##T1##_##T2##_s
#define TEST_UNARY_STRUCT_DECL(T1, T2) struct TEST_UNARY_STRUCT_NAME(T1, T2)
#define TEST_UNARY_STRUCT(T1, T2)       \
  struct TEST_UNARY_STRUCT_NAME(T1, T2) \
    {                                   \
      T1 to;                            \
      T2 from;                          \
    };

#define TEST_UNARY_DATA(T1, T2)      t_##T1##_##T2##_s
#define TEST_UNARY_DATA_WRAP(T1, T2) TEST_UNARY_DATA(T1, T2)

TEST_UNARY_STRUCT (uint8_t, uint16_t)
TEST_UNARY_STRUCT (uint8_t, uint32_t)
TEST_UNARY_STRUCT (uint8_t, uint64_t)
TEST_UNARY_STRUCT (uint16_t, uint32_t)
TEST_UNARY_STRUCT (uint16_t, uint64_t)
TEST_UNARY_STRUCT (uint32_t, uint64_t)

TEST_UNARY_STRUCT_DECL(uint8_t, uint16_t) \
  TEST_UNARY_DATA(uint8_t, uint16_t)[] =
{
  {  0,     0},
  {  2,     2},
  {254,   254},
  {255,   255},
  {255,   256},
  {255, 65534},
  {255, 65535},
};

TEST_UNARY_STRUCT_DECL(uint8_t, uint32_t) \
  TEST_UNARY_DATA(uint8_t, uint32_t)[] =
{
  {  0,          0},
  {  2,          2},
  {254,        254},
  {255,        255},
  {255,        256},
  {255,      65534},
  {255,      65535},
  {255,      65536},
  {255, 4294967294},
  {255, 4294967295},
};

TEST_UNARY_STRUCT_DECL(uint8_t, uint64_t) \
  TEST_UNARY_DATA(uint8_t, uint64_t)[] =
{
  {  0,                     0},
  {  2,                     2},
  {254,                   254},
  {255,                   255},
  {255,                   256},
  {255,                 65534},
  {255,                 65535},
  {255,                 65536},
  {255,            4294967294},
  {255,            4294967295},
  {255,            4294967296},
  {255, 18446744073709551614u},
  {255, 18446744073709551615u},
};

TEST_UNARY_STRUCT_DECL(uint16_t, uint32_t) \
  TEST_UNARY_DATA(uint16_t, uint32_t)[] =
{
  {    0,          0},
  {    5,          5},
  {65534,      65534},
  {65535,      65535},
  {65535,      65536},
  {65535, 4294967294},
  {65535, 4294967295},
};

TEST_UNARY_STRUCT_DECL(uint16_t, uint64_t) \
  TEST_UNARY_DATA(uint16_t, uint64_t)[] =
{
  {    0,                     0},
  {    5,                     5},
  {65534,                 65534},
  {65535,                 65535},
  {65535,                 65536},
  {65535,            4294967294},
  {65535,            4294967295},
  {65535,            4294967296},
  {65535, 18446744073709551614u},
  {65535, 18446744073709551615u},
};

TEST_UNARY_STRUCT_DECL(uint32_t, uint64_t) \
  TEST_UNARY_DATA(uint32_t, uint64_t)[] =
{
  {    0,                          0},
  {    9,                          9},
  {4294967294,            4294967294},
  {4294967295,            4294967295},
  {4294967295,            4294967296},
  {4294967295, 18446744073709551614u},
  {4294967295, 18446744073709551615u},
};

#endif
