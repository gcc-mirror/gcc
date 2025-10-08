#ifndef HAVE_DEFINED_VX_WIDEN_DATA_H
#define HAVE_DEFINED_VX_WIDEN_DATA_H

#define N 16

#define DEF_BINARY_WIDEN_STRUCT_0_NAME(WT, NT, NAME) \
  binary_widen_##WT##_##NT##_##NAME##_s_0
#define DEF_BINARY_WIDEN_STRUCT_0_NAME_WRAP(WT, NT, NAME) \
  DEF_BINARY_WIDEN_STRUCT_0_NAME(WT, NT, NAME)

#define DEF_BINARY_WIDEN_STRUCT_0_TYPE(WT, NT, NAME) \
  struct DEF_BINARY_WIDEN_STRUCT_0_NAME_WRAP(WT, NT, NAME)
#define DEF_BINARY_WIDEN_STRUCT_0_TYPE_WRAP(WT, NT, NAME) \
  DEF_BINARY_WIDEN_STRUCT_0_TYPE(WT, NT, NAME)

#define DEF_BINARY_WIDEN_STRUCT_0_VAR(WT, NT, NAME) \
  binary_widen_##WT##_##NT##_##NAME##_data_0
#define DEF_BINARY_WIDEN_STRUCT_0_VAR_WRAP(WT, NT, NAME) \
  DEF_BINARY_WIDEN_STRUCT_0_VAR(WT, NT, NAME)

#define DEF_BINARY_WIDEN_STRUCT_0_DECL(WT, NT, NAME) \
  DEF_BINARY_WIDEN_STRUCT_0_TYPE_WRAP(WT, NT, NAME)  \
  DEF_BINARY_WIDEN_STRUCT_0_VAR_WRAP(WT, NT, NAME)
#define DEF_BINARY_WIDEN_STRUCT_0_DECL_WRAP(WT, NT, NAME) \
  DEF_BINARY_WIDEN_STRUCT_0_DECL(WT, NT, NAME)

#define DEF_BINARY_WIDEN_STRUCT_1_NAME(WT, NT, NAME) \
  binary_widen_##WT##_##NT##_##NAME##_s_1
#define DEF_BINARY_WIDEN_STRUCT_1_NAME_WRAP(WT, NT, NAME) \
  DEF_BINARY_WIDEN_STRUCT_1_NAME(WT, NT, NAME)

#define DEF_BINARY_WIDEN_STRUCT_1_TYPE(WT, NT, NAME) \
  struct DEF_BINARY_WIDEN_STRUCT_1_NAME_WRAP(WT, NT, NAME)
#define DEF_BINARY_WIDEN_STRUCT_1_TYPE_WRAP(WT, NT, NAME) \
  DEF_BINARY_WIDEN_STRUCT_1_TYPE(WT, NT, NAME)

#define DEF_BINARY_WIDEN_STRUCT_1_VAR(WT, NT, NAME) \
  binary_widen_##WT##_##NT##_##NAME##_data_1
#define DEF_BINARY_WIDEN_STRUCT_1_VAR_WRAP(WT, NT, NAME) \
  DEF_BINARY_WIDEN_STRUCT_1_VAR(WT, NT, NAME)

#define DEF_BINARY_WIDEN_STRUCT_1_DECL(WT, NT, NAME) \
  DEF_BINARY_WIDEN_STRUCT_1_TYPE_WRAP(WT, NT, NAME)  \
  DEF_BINARY_WIDEN_STRUCT_1_VAR_WRAP(WT, NT, NAME)
#define DEF_BINARY_WIDEN_STRUCT_1_DECL_WRAP(WT, NT, NAME) \
  DEF_BINARY_WIDEN_STRUCT_1_DECL(WT, NT, NAME)

#define DEF_BINARY_WIDEN_STRUCT_0(WT, NT, NAME)            \
  DEF_BINARY_WIDEN_STRUCT_0_TYPE_WRAP(WT, NT, NAME)        \
    {                                                      \
      NT vs2[N];                                           \
      NT rs1;                                              \
      WT expect[N];                                        \
      WT vd[N];                                            \
    };
#define DEF_BINARY_WIDEN_STRUCT_0_WRAP(WT, NT, NAME)       \
  DEF_BINARY_WIDEN_STRUCT_0(WT, NT, NAME)

DEF_BINARY_WIDEN_STRUCT_0_WRAP(uint64_t, uint32_t, add)
DEF_BINARY_WIDEN_STRUCT_0_WRAP(uint64_t, uint32_t, sub)
DEF_BINARY_WIDEN_STRUCT_0_WRAP(uint64_t, uint32_t, mul)

#define DEF_BINARY_WIDEN_STRUCT_1(WT, NT, NAME)            \
  DEF_BINARY_WIDEN_STRUCT_1_TYPE_WRAP(WT, NT, NAME)        \
    {                                                      \
      WT vs2[N];                                           \
      NT rs1;                                              \
      WT expect[N];                                        \
      WT vd[N];                                            \
    };
#define DEF_BINARY_WIDEN_STRUCT_1_WRAP(WT, NT, NAME)       \
  DEF_BINARY_WIDEN_STRUCT_1(WT, NT, NAME)

DEF_BINARY_WIDEN_STRUCT_1_WRAP(uint64_t, uint32_t, add)
DEF_BINARY_WIDEN_STRUCT_1_WRAP(uint64_t, uint32_t, sub)

DEF_BINARY_WIDEN_STRUCT_0_DECL_WRAP(uint64_t, uint32_t, add)[] = {
  {
    /* vs2 */
    {
               1,          1,          1,          1,
               0,          0,          0,          0,
      2147483647, 2147483647, 2147483647, 2147483647,
      2147483648, 2147483648, 2147483648, 2147483648,
    },
    /* rs1 */
    2147483647,
    /* expect */
    {
      2147483648, 2147483648, 2147483648, 2147483648,
      2147483647, 2147483647, 2147483647, 2147483647,
      4294967294, 4294967294, 4294967294, 4294967294,
      4294967295, 4294967295, 4294967295, 4294967295,
    },
  },
  {
    /* vs2 */
    {
               1,          1,          1,          1,
               0,          0,          0,          0,
      4294967295, 4294967295, 4294967295, 4294967295,
      4294967294, 4294967294, 4294967294, 4294967294,
    },
    /* rs1 */
    4294967295,
    /* expect */
    {
      4294967296ull, 4294967296ull, 4294967296ull, 4294967296ull,
      4294967295ull, 4294967295ull, 4294967295ull, 4294967295ull,
      8589934590ull, 8589934590ull, 8589934590ull, 8589934590ull,
      8589934589ull, 8589934589ull, 8589934589ull, 8589934589ull,
    },
  },
};

DEF_BINARY_WIDEN_STRUCT_0_DECL_WRAP(uint64_t, uint32_t, sub)[] = {
  {
    /* vs2 */
    {
      2147483648, 2147483648, 2147483648, 2147483648,
      2147483647, 2147483647, 2147483647, 2147483647,
      4294967294, 4294967294, 4294967294, 4294967294,
      4294967295, 4294967295, 4294967295, 4294967295,
    },
    /* rs1 */
    2147483647,
    /* expect */
    {
               1,          1,          1,          1,
               0,          0,          0,          0,
      2147483647, 2147483647, 2147483647, 2147483647,
      2147483648, 2147483648, 2147483648, 2147483648,
    },
  },
  {
    /* vs2 */
    {
      4294967295ull, 4294967295ull, 4294967295ull, 4294967295ull,
      4294967294ull, 4294967294ull, 4294967294ull, 4294967294ull,
                  1,             1,             1,             1,
                  0,             0,             0,             0,
    },
    /* rs1 */
    4294967295,
    /* expect */
    {
                           0,                       0,                       0,                       0,
     18446744073709551615ull, 18446744073709551615ull, 18446744073709551615ull, 18446744073709551615ull,
     18446744069414584322ull, 18446744069414584322ull, 18446744069414584322ull, 18446744069414584322ull, 
     18446744069414584321ull, 18446744069414584321ull, 18446744069414584321ull, 18446744069414584321ull, 
    },
  },
};

DEF_BINARY_WIDEN_STRUCT_0_DECL_WRAP(uint64_t, uint32_t, mul)[] = {
  {
    /* vs2 */
    {
      1, 1, 1, 1,
      0, 0, 0, 0,
      2, 2, 2, 2,
      9, 9, 9, 9,
    },
    /* rs1 */
    2147483647,
    /* expect */
    {
          2147483647,     2147483647,     2147483647,     2147483647,
                   0,              0,              0,              0,
          4294967294,     4294967294,     4294967294,     4294967294,
      19327352823ull, 19327352823ull, 19327352823ull, 19327352823ull,
    },
  },
  {
    /* vs2 */
    {
                  1,             1,             1,             1,
                  0,             0,             0,             0,
      4294967295ull, 4294967295ull, 4294967295ull, 4294967295ull,
      4294967294ull, 4294967294ull, 4294967294ull, 4294967294ull,
    },
    /* rs1 */
    4294967295,
    /* expect */
    {
                4294967295ull,           4294967295ull,           4294967295ull,           4294967295ull,
                            0,                       0,                       0,                       0,
      18446744065119617025ull, 18446744065119617025ull, 18446744065119617025ull, 18446744065119617025ull,
      18446744060824649730ull, 18446744060824649730ull, 18446744060824649730ull, 18446744060824649730ull,
    },
  },
};

DEF_BINARY_WIDEN_STRUCT_1_DECL_WRAP(uint64_t, uint32_t, add)[] = {
  {
    /* vs2 */
    {
               1,          1,          1,          1,
               0,          0,          0,          0,
      2147483647, 2147483647, 2147483647, 2147483647,
      2147483649, 2147483649, 2147483649, 2147483649,
    },
    /* rs1 */
    2147483647,
    /* expect */
    {
         2147483648,    2147483648,    2147483648,    2147483648,
         2147483647,    2147483647,    2147483647,    2147483647,
         4294967294,    4294967294,    4294967294,    4294967294,
      4294967296ull, 4294967296ull, 4294967296ull, 4294967296ull,
    },
  },
  {
    /* vs2 */
    {
                  1,             1,             1,             1,
                  0,             0,             0,             0,
      4294967295ull, 4294967295ull, 4294967295ull, 4294967295ull,
      4294967296ull, 4294967296ull, 4294967296ull, 4294967296ull,
    },
    /* rs1 */
    4294967295,
    /* expect */
    {
      4294967296ull, 4294967296ull, 4294967296ull, 4294967296ull,
      4294967295ull, 4294967295ull, 4294967295ull, 4294967295ull,
      8589934590ull, 8589934590ull, 8589934590ull, 8589934590ull,
      8589934591ull, 8589934591ull, 8589934591ull, 8589934591ull,
    },
  },
};

DEF_BINARY_WIDEN_STRUCT_1_DECL_WRAP(uint64_t, uint32_t, sub)[] = {
  {
    /* vs2 */
    {
         2147483648,    2147483648,    2147483648,    2147483648,
         2147483647,    2147483647,    2147483647,    2147483647,
         4294967294,    4294967294,    4294967294,    4294967294,
      4294967296ull, 4294967296ull, 4294967296ull, 4294967296ull,
    },
    /* rs1 */
    2147483647,
    /* expect */
    {
               1,          1,          1,          1,
               0,          0,          0,          0,
      2147483647, 2147483647, 2147483647, 2147483647,
      2147483649, 2147483649, 2147483649, 2147483649,
    },
  },
  {
    /* vs2 */
    {
      4294967296ull, 4294967296ull, 4294967296ull, 4294967296ull,
      4294967295ull, 4294967295ull, 4294967295ull, 4294967295ull,
      8589934590ull, 8589934590ull, 8589934590ull, 8589934590ull,
      8589934591ull, 8589934591ull, 8589934591ull, 8589934591ull,
    },
    /* rs1 */
    4294967295,
    /* expect */
    {
                  1,             1,             1,             1,
                  0,             0,             0,             0,
      4294967295ull, 4294967295ull, 4294967295ull, 4294967295ull,
      4294967296ull, 4294967296ull, 4294967296ull, 4294967296ull,
    },
  },
};

#endif
