#ifndef HAVE_SAT_ARITH
#define HAVE_SAT_ARITH

#include <stdint-gcc.h>
#include <stdbool.h>

/******************************************************************************/
/* Saturation Add (unsigned and signed)                                       */
/******************************************************************************/
#define DEF_SAT_U_ADD_FMT_1(T)             \
T __attribute__((noinline))                \
sat_u_add_##T##_fmt_1 (T x, T y)           \
{                                          \
  return (x + y) | (-(T)((T)(x + y) < x)); \
}

#define DEF_SAT_U_ADD_FMT_2(T)           \
T __attribute__((noinline))              \
sat_u_add_##T##_fmt_2 (T x, T y)         \
{                                        \
  return (T)(x + y) >= x ? (x + y) : -1; \
}

#define DEF_SAT_U_ADD_FMT_3(T)                      \
T __attribute__((noinline))                         \
sat_u_add_##T##_fmt_3 (T x, T y)                    \
{                                                   \
  T ret;                                            \
  T overflow = __builtin_add_overflow (x, y, &ret); \
  return (T)(-overflow) | ret;                      \
}

#define DEF_SAT_U_ADD_FMT_4(T)                           \
T __attribute__((noinline))                              \
sat_u_add_##T##_fmt_4 (T x, T y)                         \
{                                                        \
  T ret;                                                 \
  return __builtin_add_overflow (x, y, &ret) ? -1 : ret; \
}

#define DEF_SAT_U_ADD_FMT_5(T)                                \
T __attribute__((noinline))                                   \
sat_u_add_##T##_fmt_5 (T x, T y)                              \
{                                                             \
  T ret;                                                      \
  return __builtin_add_overflow (x, y, &ret) == 0 ? ret : -1; \
}

#define DEF_SAT_U_ADD_FMT_6(T)          \
T __attribute__((noinline))             \
sat_u_add_##T##_fmt_6 (T x, T y)        \
{                                       \
  return (T)(x + y) < x ? -1 : (x + y); \
}

#define RUN_SAT_U_ADD_FMT_1(T, x, y) sat_u_add_##T##_fmt_1(x, y)
#define RUN_SAT_U_ADD_FMT_2(T, x, y) sat_u_add_##T##_fmt_2(x, y)
#define RUN_SAT_U_ADD_FMT_3(T, x, y) sat_u_add_##T##_fmt_3(x, y)
#define RUN_SAT_U_ADD_FMT_4(T, x, y) sat_u_add_##T##_fmt_4(x, y)
#define RUN_SAT_U_ADD_FMT_5(T, x, y) sat_u_add_##T##_fmt_5(x, y)
#define RUN_SAT_U_ADD_FMT_6(T, x, y) sat_u_add_##T##_fmt_6(x, y)

#define DEF_SAT_U_ADD_IMM_FMT_1(T, IMM)      \
T __attribute__((noinline))                  \
sat_u_add_imm##IMM##_##T##_fmt_1 (T x)       \
{                                            \
  return (T)(x + IMM) >= x ? (x + IMM) : -1; \
}

#define DEF_SAT_U_ADD_IMM_FMT_2(T, IMM)     \
T __attribute__((noinline))                 \
sat_u_add_imm##IMM##_##T##_fmt_2 (T x)      \
{                                           \
  return (T)(x + IMM) < x ? -1 : (x + IMM); \
}

#define DEF_SAT_U_ADD_IMM_FMT_3(T, IMM)                    \
T __attribute__((noinline))                                \
sat_u_add_imm##IMM##_##T##_fmt_3 (T x)                     \
{                                                          \
  T ret;                                                   \
  return __builtin_add_overflow (x, IMM, &ret) ? -1 : ret; \
}

#define DEF_SAT_U_ADD_IMM_FMT_4(T, IMM)                         \
T __attribute__((noinline))                                     \
sat_u_add_imm##IMM##_##T##_fmt_4 (T x)                          \
{                                                               \
  T ret;                                                        \
  return __builtin_add_overflow (x, IMM, &ret) == 0 ? ret : -1; \
}

#define DEF_SAT_U_ADD_IMM_TYPE_CHECK_FMT_1(T, IMM)         \
T __attribute__((noinline))                                \
sat_u_add_imm_type_check##_##T##_fmt_1 (T x)               \
{                                                          \
  T ret;                                                   \
  return __builtin_add_overflow (x, IMM, &ret) ? -1 : ret; \
}

#define DEF_SAT_U_ADD_IMM_TYPE_CHECK_FMT_2(T, IMM)              \
T __attribute__((noinline))                                     \
sat_u_add_imm_type_check##_##T##_fmt_2 (T x)                    \
{                                                               \
  T ret;                                                        \
  return __builtin_add_overflow (x, IMM, &ret) == 0 ? ret : -1; \
}

#define RUN_SAT_U_ADD_IMM_FMT_1(T, x, IMM, expect) \
  if (sat_u_add_imm##IMM##_##T##_fmt_1(x) != expect) __builtin_abort ()

#define RUN_SAT_U_ADD_IMM_FMT_2(T, x, IMM, expect) \
  if (sat_u_add_imm##IMM##_##T##_fmt_2(x) != expect) __builtin_abort ()

#define RUN_SAT_U_ADD_IMM_FMT_3(T, x, IMM, expect) \
  if (sat_u_add_imm##IMM##_##T##_fmt_3(x) != expect) __builtin_abort ()

#define RUN_SAT_U_ADD_IMM_FMT_4(T, x, IMM, expect) \
  if (sat_u_add_imm##IMM##_##T##_fmt_4(x) != expect) __builtin_abort ()

#define DEF_SAT_S_ADD_FMT_1(T, UT, MIN, MAX) \
T __attribute__((noinline))                  \
sat_s_add_##T##_fmt_1 (T x, T y)             \
{                                            \
  T sum = (UT)x + (UT)y;                     \
  return (x ^ y) < 0                         \
    ? sum                                    \
    : (sum ^ x) >= 0                         \
      ? sum                                  \
      : x < 0 ? MIN : MAX;                   \
}
#define DEF_SAT_S_ADD_FMT_1_WRAP(T, UT, MIN, MAX) \
  DEF_SAT_S_ADD_FMT_1(T, UT, MIN, MAX)

#define DEF_SAT_S_ADD_FMT_2(T, UT, MIN, MAX) \
T __attribute__((noinline))                  \
sat_s_add_##T##_fmt_2 (T x, T y)             \
{                                            \
  T sum = (UT)x + (UT)y;                     \
  if ((x ^ y) < 0 || (sum ^ x) >= 0)         \
    return sum;                              \
  return x < 0 ? MIN : MAX;                  \
}
#define DEF_SAT_S_ADD_FMT_2_WRAP(T, UT, MIN, MAX) \
  DEF_SAT_S_ADD_FMT_2(T, UT, MIN, MAX)

#define DEF_SAT_S_ADD_FMT_3(T, UT, MIN, MAX)           \
T __attribute__((noinline))                            \
sat_s_add_##T##_fmt_3 (T x, T y)                       \
{                                                      \
  T sum;                                               \
  bool overflow = __builtin_add_overflow (x, y, &sum); \
  return overflow ? x < 0 ? MIN : MAX : sum;           \
}
#define DEF_SAT_S_ADD_FMT_3_WRAP(T, UT, MIN, MAX) \
  DEF_SAT_S_ADD_FMT_3(T, UT, MIN, MAX)

#define DEF_SAT_S_ADD_FMT_4(T, UT, MIN, MAX)           \
T __attribute__((noinline))                            \
sat_s_add_##T##_fmt_4 (T x, T y)                       \
{                                                      \
  T sum;                                               \
  bool overflow = __builtin_add_overflow (x, y, &sum); \
  return !overflow ? sum : x < 0 ? MIN : MAX;          \
}
#define DEF_SAT_S_ADD_FMT_4_WRAP(T, UT, MIN, MAX) \
  DEF_SAT_S_ADD_FMT_4(T, UT, MIN, MAX)

#define RUN_SAT_S_ADD_FMT_1(T, x, y) sat_s_add_##T##_fmt_1(x, y)
#define RUN_SAT_S_ADD_FMT_1_WRAP(T, x, y) RUN_SAT_S_ADD_FMT_1(T, x, y)

#define RUN_SAT_S_ADD_FMT_2(T, x, y) sat_s_add_##T##_fmt_2(x, y)
#define RUN_SAT_S_ADD_FMT_2_WRAP(T, x, y) RUN_SAT_S_ADD_FMT_2(T, x, y)

#define RUN_SAT_S_ADD_FMT_3(T, x, y) sat_s_add_##T##_fmt_3(x, y)
#define RUN_SAT_S_ADD_FMT_3_WRAP(T, x, y) RUN_SAT_S_ADD_FMT_3(T, x, y)

#define RUN_SAT_S_ADD_FMT_4(T, x, y) sat_s_add_##T##_fmt_4(x, y)
#define RUN_SAT_S_ADD_FMT_4_WRAP(T, x, y) RUN_SAT_S_ADD_FMT_4(T, x, y)

#define DEF_SAT_S_ADD_IMM_FMT_1(INDEX, T, UT, IMM, MIN, MAX) \
T __attribute__((noinline))                  \
sat_s_add_imm_##T##_fmt_1##_##INDEX (T x)             \
{                                            \
  T sum = (UT)x + (UT)IMM;                     \
  return (x ^ IMM) < 0                         \
    ? sum                                    \
    : (sum ^ x) >= 0                         \
      ? sum                                  \
      : x < 0 ? MIN : MAX;                   \
}

#define RUN_SAT_S_ADD_IMM_FMT_1(INDEX, T, x, expect) \
  if (sat_s_add_imm##_##T##_fmt_1##_##INDEX(x) != expect) __builtin_abort ()

/******************************************************************************/
/* Saturation Sub (Unsigned and Signed)                                       */
/******************************************************************************/
#define DEF_SAT_U_SUB_FMT_1(T)     \
T __attribute__((noinline))        \
sat_u_sub_##T##_fmt_1 (T x, T y)   \
{                                  \
  return (x - y) & (-(T)(x >= y)); \
}

#define DEF_SAT_U_SUB_FMT_2(T)    \
T __attribute__((noinline))       \
sat_u_sub_##T##_fmt_2 (T x, T y)  \
{                                 \
  return (x - y) & (-(T)(x > y)); \
}

#define DEF_SAT_U_SUB_FMT_3(T)   \
T __attribute__((noinline))      \
sat_u_sub_##T##_fmt_3 (T x, T y) \
{                                \
  return x > y ? x - y : 0;      \
}

#define DEF_SAT_U_SUB_FMT_4(T)   \
T __attribute__((noinline))      \
sat_u_sub_##T##_fmt_4 (T x, T y) \
{                                \
  return x >= y ? x - y : 0;     \
}

#define DEF_SAT_U_SUB_FMT_5(T)   \
T __attribute__((noinline))      \
sat_u_sub_##T##_fmt_5 (T x, T y) \
{                                \
  return x < y ? 0 : x - y;      \
}

#define DEF_SAT_U_SUB_FMT_6(T)   \
T __attribute__((noinline))      \
sat_u_sub_##T##_fmt_6 (T x, T y) \
{                                \
  return x <= y ? 0 : x - y;     \
}

#define DEF_SAT_U_SUB_FMT_7(T)                      \
T __attribute__((noinline))                         \
sat_u_sub_##T##_fmt_7 (T x, T y)                    \
{                                                   \
  T ret;                                            \
  T overflow = __builtin_sub_overflow (x, y, &ret); \
  return ret & (T)(overflow - 1);                   \
}

#define DEF_SAT_U_SUB_FMT_8(T)                      \
T __attribute__((noinline))                         \
sat_u_sub_##T##_fmt_8 (T x, T y)                    \
{                                                   \
  T ret;                                            \
  T overflow = __builtin_sub_overflow (x, y, &ret); \
  return ret & (T)-(!overflow);                     \
}

#define DEF_SAT_U_SUB_FMT_9(T)                      \
T __attribute__((noinline))                         \
sat_u_sub_##T##_fmt_9 (T x, T y)                    \
{                                                   \
  T ret;                                            \
  T overflow = __builtin_sub_overflow (x, y, &ret); \
  return overflow ? 0 : ret;                        \
}

#define DEF_SAT_U_SUB_FMT_10(T)                     \
T __attribute__((noinline))                         \
sat_u_sub_##T##_fmt_10 (T x, T y)                   \
{                                                   \
  T ret;                                            \
  T overflow = __builtin_sub_overflow (x, y, &ret); \
  return !overflow ? ret : 0;                       \
}

#define DEF_SAT_U_SUB_FMT_11(T)                        \
T __attribute__((noinline))                            \
sat_u_sub_##T##_fmt_11 (T x, T y)                      \
{                                                      \
  T ret;                                               \
  bool overflow = __builtin_sub_overflow (x, y, &ret); \
  return overflow ? 0 : ret;                           \
}

#define DEF_SAT_U_SUB_FMT_12(T)                        \
T __attribute__((noinline))                            \
sat_u_sub_##T##_fmt_12 (T x, T y)                      \
{                                                      \
  T ret;                                               \
  bool overflow = __builtin_sub_overflow (x, y, &ret); \
  return !overflow ? ret : 0;                          \
}

#define DEF_SAT_U_SUB_IMM_FMT_1(T, IMM) \
T __attribute__((noinline))             \
sat_u_sub_imm##IMM##_##T##_fmt_1 (T y)  \
{                                       \
  return (T)IMM >= y ? (T)IMM - y : 0;  \
}

#define DEF_SAT_U_SUB_IMM_FMT_2(T, IMM) \
T __attribute__((noinline))             \
sat_u_sub_imm##IMM##_##T##_fmt_2 (T x)  \
{                                       \
  return x >= (T)IMM ? x - (T)IMM : 0;  \
}

#define DEF_SAT_U_SUB_IMM_FMT_3(T, IMM) \
T __attribute__((noinline))             \
sat_u_sub_imm##IMM##_##T##_fmt_3 (T y)  \
{                                       \
  return (T)IMM > y ? (T)IMM - y : 0;   \
}

#define DEF_SAT_U_SUB_IMM_FMT_4(T, IMM) \
T __attribute__((noinline))             \
sat_u_sub_imm##IMM##_##T##_fmt_4 (T x)  \
{                                       \
  return x > (T)IMM ? x - (T)IMM : 0;   \
}

#define RUN_SAT_U_SUB_FMT_1(T, x, y) sat_u_sub_##T##_fmt_1(x, y)
#define RUN_SAT_U_SUB_FMT_2(T, x, y) sat_u_sub_##T##_fmt_2(x, y)
#define RUN_SAT_U_SUB_FMT_3(T, x, y) sat_u_sub_##T##_fmt_3(x, y)
#define RUN_SAT_U_SUB_FMT_4(T, x, y) sat_u_sub_##T##_fmt_4(x, y)
#define RUN_SAT_U_SUB_FMT_5(T, x, y) sat_u_sub_##T##_fmt_5(x, y)
#define RUN_SAT_U_SUB_FMT_6(T, x, y) sat_u_sub_##T##_fmt_6(x, y)
#define RUN_SAT_U_SUB_FMT_7(T, x, y) sat_u_sub_##T##_fmt_7(x, y)
#define RUN_SAT_U_SUB_FMT_8(T, x, y) sat_u_sub_##T##_fmt_8(x, y)
#define RUN_SAT_U_SUB_FMT_9(T, x, y) sat_u_sub_##T##_fmt_9(x, y)
#define RUN_SAT_U_SUB_FMT_10(T, x, y) sat_u_sub_##T##_fmt_10(x, y)
#define RUN_SAT_U_SUB_FMT_11(T, x, y) sat_u_sub_##T##_fmt_11(x, y)
#define RUN_SAT_U_SUB_FMT_12(T, x, y) sat_u_sub_##T##_fmt_12(x, y)

#define RUN_SAT_U_SUB_IMM_FMT_1(T, IMM, y, expect) \
  if (sat_u_sub_imm##IMM##_##T##_fmt_1(y) != expect) __builtin_abort ()
#define RUN_SAT_U_SUB_IMM_FMT_2(T, x, IMM, expect) \
  if (sat_u_sub_imm##IMM##_##T##_fmt_2(x) != expect) __builtin_abort ()
#define RUN_SAT_U_SUB_IMM_FMT_3(T, IMM, y, expect) \
  if (sat_u_sub_imm##IMM##_##T##_fmt_3(y) != expect) __builtin_abort ()
#define RUN_SAT_U_SUB_IMM_FMT_4(T, x, IMM, expect) \
  if (sat_u_sub_imm##IMM##_##T##_fmt_4(x) != expect) __builtin_abort ()

#define DEF_SAT_U_SUB_IMM_TYPE_CHECK_FMT_1(INDEX, T, IMM) \
T __attribute__((noinline))                               \
sat_u_sub_imm_type_check##_##INDEX##_##T##_fmt_1 (T y)    \
{                                                         \
  return IMM >= y ? IMM - y : 0;                          \
}

#define DEF_SAT_U_SUB_IMM_TYPE_CHECK_FMT_2(INDEX, T, IMM) \
T __attribute__((noinline))                               \
sat_u_sub_imm_type_check##_##INDEX##_##T##_fmt_2 (T y)    \
{                                                         \
  return IMM > y ? IMM - y : 0;                           \
}

#define DEF_SAT_U_SUB_IMM_TYPE_CHECK_FMT_3(INDEX, T, IMM) \
T __attribute__((noinline))                               \
sat_u_sub_imm_type_check##_##INDEX##_##T##_fmt_3 (T x)    \
{                                                         \
  return x >= IMM ? x - IMM : 0;                          \
}

#define DEF_SAT_U_SUB_IMM_TYPE_CHECK_FMT_4(INDEX, T, IMM) \
T __attribute__((noinline))                               \
sat_u_sub_imm_type_check##_##INDEX##_##T##_fmt_4 (T x)    \
{                                                         \
  return x > IMM ? x - IMM : 0;                           \
}

#define DEF_SAT_S_SUB_FMT_1(T, UT, MIN, MAX) \
T __attribute__((noinline))                  \
sat_s_sub_##T##_fmt_1 (T x, T y)             \
{                                            \
  T minus = (UT)x - (UT)y;                   \
  return (x ^ y) >= 0                        \
    ? minus                                  \
    : (minus ^ x) >= 0                       \
      ? minus                                \
      : x < 0 ? MIN : MAX;                   \
}
#define DEF_SAT_S_SUB_FMT_1_WRAP(T, UT, MIN, MAX) \
  DEF_SAT_S_SUB_FMT_1(T, UT, MIN, MAX)

#define DEF_SAT_S_SUB_FMT_2(T, UT, MIN, MAX) \
T __attribute__((noinline))                  \
sat_s_sub_##T##_fmt_2 (T x, T y)             \
{                                            \
  T minus = (UT)x - (UT)y;                   \
  if ((x ^ y) >= 0 || (minus ^ x) >= 0)      \
    return minus;                            \
  return x < 0 ? MIN : MAX;                  \
}
#define DEF_SAT_S_SUB_FMT_2_WRAP(T, UT, MIN, MAX) \
  DEF_SAT_S_SUB_FMT_2(T, UT, MIN, MAX)

#define DEF_SAT_S_SUB_FMT_3(T, UT, MIN, MAX)             \
T __attribute__((noinline))                              \
sat_s_sub_##T##_fmt_3 (T x, T y)                         \
{                                                        \
  T minus;                                               \
  bool overflow = __builtin_sub_overflow (x, y, &minus); \
  return overflow ? x < 0 ? MIN : MAX : minus;           \
}
#define DEF_SAT_S_SUB_FMT_3_WRAP(T, UT, MIN, MAX) \
  DEF_SAT_S_SUB_FMT_3(T, UT, MIN, MAX)

#define DEF_SAT_S_SUB_FMT_4(T, UT, MIN, MAX)           \
T __attribute__((noinline))                            \
sat_s_sub_##T##_fmt_4 (T x, T y)                       \
{                                                      \
  T minus;                                               \
  bool overflow = __builtin_sub_overflow (x, y, &minus); \
  return !overflow ? minus : x < 0 ? MIN : MAX;          \
}
#define DEF_SAT_S_SUB_FMT_4_WRAP(T, UT, MIN, MAX) \
  DEF_SAT_S_SUB_FMT_4(T, UT, MIN, MAX)

#define RUN_SAT_S_SUB_FMT_1(T, x, y) sat_s_sub_##T##_fmt_1(x, y)
#define RUN_SAT_S_SUB_FMT_1_WRAP(T, x, y) RUN_SAT_S_SUB_FMT_1(T, x, y)

#define RUN_SAT_S_SUB_FMT_2(T, x, y) sat_s_sub_##T##_fmt_2(x, y)
#define RUN_SAT_S_SUB_FMT_2_WRAP(T, x, y) RUN_SAT_S_SUB_FMT_2(T, x, y)

#define RUN_SAT_S_SUB_FMT_3(T, x, y) sat_s_sub_##T##_fmt_3(x, y)
#define RUN_SAT_S_SUB_FMT_3_WRAP(T, x, y) RUN_SAT_S_SUB_FMT_3(T, x, y)

#define RUN_SAT_S_SUB_FMT_4(T, x, y) sat_s_sub_##T##_fmt_4(x, y)
#define RUN_SAT_S_SUB_FMT_4_WRAP(T, x, y) RUN_SAT_S_SUB_FMT_4(T, x, y)

/******************************************************************************/
/* Saturation Truncate (unsigned and signed)                                  */
/******************************************************************************/

#define DEF_SAT_U_TRUNC_FMT_1(NT, WT)    \
NT __attribute__((noinline))             \
sat_u_trunc_##WT##_to_##NT##_fmt_1 (WT x) \
{                                        \
  bool overflow = x > (WT)(NT)(-1);      \
  return ((NT)x) | (NT)-overflow;        \
}
#define DEF_SAT_U_TRUNC_FMT_1_WRAP(NT, WT) DEF_SAT_U_TRUNC_FMT_1(NT, WT)

#define DEF_SAT_U_TRUNC_FMT_2(NT, WT)    \
NT __attribute__((noinline))             \
sat_u_trunc_##WT##_to_##NT##_fmt_2 (WT x) \
{                                        \
  WT max = (WT)(NT)-1;                   \
  return x > max ? (NT) max : (NT)x;     \
}
#define DEF_SAT_U_TRUNC_FMT_2_WRAP(NT, WT) DEF_SAT_U_TRUNC_FMT_2(NT, WT)

#define DEF_SAT_U_TRUNC_FMT_3(NT, WT)    \
NT __attribute__((noinline))             \
sat_u_trunc_##WT##_to_##NT##_fmt_3 (WT x) \
{                                        \
  WT max = (WT)(NT)-1;                   \
  return x <= max ? (NT)x : (NT) max;    \
}
#define DEF_SAT_U_TRUNC_FMT_3_WRAP(NT, WT) DEF_SAT_U_TRUNC_FMT_3(NT, WT)

#define DEF_SAT_U_TRUNC_FMT_4(NT, WT)          \
NT __attribute__((noinline))                   \
sat_u_trunc_##WT##_to_##NT##_fmt_4 (WT x)      \
{                                              \
  bool not_overflow = x <= (WT)(NT)(-1);       \
  return ((NT)x) | (NT)((NT)not_overflow - 1); \
}
#define DEF_SAT_U_TRUNC_FMT_4_WRAP(NT, WT) DEF_SAT_U_TRUNC_FMT_4(NT, WT)

#define RUN_SAT_U_TRUNC_FMT_1(NT, WT, x) sat_u_trunc_##WT##_to_##NT##_fmt_1 (x)
#define RUN_SAT_U_TRUNC_FMT_1_WRAP(NT, WT, x) RUN_SAT_U_TRUNC_FMT_1(NT, WT, x)

#define RUN_SAT_U_TRUNC_FMT_2(NT, WT, x) sat_u_trunc_##WT##_to_##NT##_fmt_2 (x)
#define RUN_SAT_U_TRUNC_FMT_2_WRAP(NT, WT, x) RUN_SAT_U_TRUNC_FMT_2(NT, WT, x)

#define RUN_SAT_U_TRUNC_FMT_3(NT, WT, x) sat_u_trunc_##WT##_to_##NT##_fmt_3 (x)
#define RUN_SAT_U_TRUNC_FMT_3_WRAP(NT, WT, x) RUN_SAT_U_TRUNC_FMT_3(NT, WT, x)

#define RUN_SAT_U_TRUNC_FMT_4(NT, WT, x) sat_u_trunc_##WT##_to_##NT##_fmt_4 (x)
#define RUN_SAT_U_TRUNC_FMT_4_WRAP(NT, WT, x) RUN_SAT_U_TRUNC_FMT_4(NT, WT, x)

#define DEF_SAT_S_TRUNC_FMT_1(NT, WT, NT_MIN, NT_MAX) \
NT __attribute__((noinline))                          \
sat_s_trunc_##WT##_to_##NT##_fmt_1 (WT x)             \
{                                                     \
  NT trunc = (NT)x;                                   \
  return (WT)NT_MIN <= x && x <= (WT)NT_MAX           \
    ? trunc                                           \
    : x < 0 ? NT_MIN : NT_MAX;                        \
}
#define DEF_SAT_S_TRUNC_FMT_1_WRAP(NT, WT, NT_MIN, NT_MAX) \
  DEF_SAT_S_TRUNC_FMT_1(NT, WT, NT_MIN, NT_MAX)

#define DEF_SAT_S_TRUNC_FMT_2(NT, WT, NT_MIN, NT_MAX) \
NT __attribute__((noinline))                          \
sat_s_trunc_##WT##_to_##NT##_fmt_2 (WT x)             \
{                                                     \
  NT trunc = (NT)x;                                   \
  return (WT)NT_MIN < x && x < (WT)NT_MAX             \
    ? trunc                                           \
    : x < 0 ? NT_MIN : NT_MAX;                        \
}
#define DEF_SAT_S_TRUNC_FMT_2_WRAP(NT, WT, NT_MIN, NT_MAX) \
  DEF_SAT_S_TRUNC_FMT_2(NT, WT, NT_MIN, NT_MAX)

#define DEF_SAT_S_TRUNC_FMT_3(NT, WT, NT_MIN, NT_MAX) \
NT __attribute__((noinline))                          \
sat_s_trunc_##WT##_to_##NT##_fmt_3 (WT x)             \
{                                                     \
  NT trunc = (NT)x;                                   \
  return (WT)NT_MIN < x && x <= (WT)NT_MAX            \
    ? trunc                                           \
    : x < 0 ? NT_MIN : NT_MAX;                        \
}
#define DEF_SAT_S_TRUNC_FMT_3_WRAP(NT, WT, NT_MIN, NT_MAX) \
  DEF_SAT_S_TRUNC_FMT_3(NT, WT, NT_MIN, NT_MAX)

#define DEF_SAT_S_TRUNC_FMT_4(NT, WT, NT_MIN, NT_MAX) \
NT __attribute__((noinline))                          \
sat_s_trunc_##WT##_to_##NT##_fmt_4 (WT x)             \
{                                                     \
  NT trunc = (NT)x;                                   \
  return (WT)NT_MIN <= x && x < (WT)NT_MAX            \
    ? trunc                                           \
    : x < 0 ? NT_MIN : NT_MAX;                        \
}
#define DEF_SAT_S_TRUNC_FMT_4_WRAP(NT, WT, NT_MIN, NT_MAX) \
  DEF_SAT_S_TRUNC_FMT_4(NT, WT, NT_MIN, NT_MAX)

#define DEF_SAT_S_TRUNC_FMT_5(NT, WT, NT_MIN, NT_MAX) \
NT __attribute__((noinline))                          \
sat_s_trunc_##WT##_to_##NT##_fmt_5 (WT x)             \
{                                                     \
  NT trunc = (NT)x;                                   \
  return (WT)NT_MIN > x || x > (WT)NT_MAX             \
    ? x < 0 ? NT_MIN : NT_MAX                         \
    : trunc;                                          \
}
#define DEF_SAT_S_TRUNC_FMT_5_WRAP(NT, WT, NT_MIN, NT_MAX) \
  DEF_SAT_S_TRUNC_FMT_5(NT, WT, NT_MIN, NT_MAX)

#define DEF_SAT_S_TRUNC_FMT_6(NT, WT, NT_MIN, NT_MAX) \
NT __attribute__((noinline))                          \
sat_s_trunc_##WT##_to_##NT##_fmt_6 (WT x)             \
{                                                     \
  NT trunc = (NT)x;                                   \
  return (WT)NT_MIN >= x || x > (WT)NT_MAX            \
    ? x < 0 ? NT_MIN : NT_MAX                         \
    : trunc;                                          \
}
#define DEF_SAT_S_TRUNC_FMT_6_WRAP(NT, WT, NT_MIN, NT_MAX) \
  DEF_SAT_S_TRUNC_FMT_6(NT, WT, NT_MIN, NT_MAX)

#define DEF_SAT_S_TRUNC_FMT_7(NT, WT, NT_MIN, NT_MAX) \
NT __attribute__((noinline))                          \
sat_s_trunc_##WT##_to_##NT##_fmt_7 (WT x)             \
{                                                     \
  NT trunc = (NT)x;                                   \
  return (WT)NT_MIN >= x || x >= (WT)NT_MAX           \
    ? x < 0 ? NT_MIN : NT_MAX                         \
    : trunc;                                          \
}
#define DEF_SAT_S_TRUNC_FMT_7_WRAP(NT, WT, NT_MIN, NT_MAX) \
  DEF_SAT_S_TRUNC_FMT_7(NT, WT, NT_MIN, NT_MAX)

#define DEF_SAT_S_TRUNC_FMT_8(NT, WT, NT_MIN, NT_MAX) \
NT __attribute__((noinline))                          \
sat_s_trunc_##WT##_to_##NT##_fmt_8 (WT x)             \
{                                                     \
  NT trunc = (NT)x;                                   \
  return (WT)NT_MIN > x || x >= (WT)NT_MAX            \
    ? x < 0 ? NT_MIN : NT_MAX                         \
    : trunc;                                          \
}
#define DEF_SAT_S_TRUNC_FMT_8_WRAP(NT, WT, NT_MIN, NT_MAX) \
  DEF_SAT_S_TRUNC_FMT_8(NT, WT, NT_MIN, NT_MAX)

#define RUN_SAT_S_TRUNC_FMT_1(NT, WT, x) sat_s_trunc_##WT##_to_##NT##_fmt_1 (x)
#define RUN_SAT_S_TRUNC_FMT_1_WRAP(NT, WT, x) RUN_SAT_S_TRUNC_FMT_1(NT, WT, x)

#define RUN_SAT_S_TRUNC_FMT_2(NT, WT, x) sat_s_trunc_##WT##_to_##NT##_fmt_2 (x)
#define RUN_SAT_S_TRUNC_FMT_2_WRAP(NT, WT, x) RUN_SAT_S_TRUNC_FMT_2(NT, WT, x)

#define RUN_SAT_S_TRUNC_FMT_3(NT, WT, x) sat_s_trunc_##WT##_to_##NT##_fmt_3 (x)
#define RUN_SAT_S_TRUNC_FMT_3_WRAP(NT, WT, x) RUN_SAT_S_TRUNC_FMT_3(NT, WT, x)

#define RUN_SAT_S_TRUNC_FMT_4(NT, WT, x) sat_s_trunc_##WT##_to_##NT##_fmt_4 (x)
#define RUN_SAT_S_TRUNC_FMT_4_WRAP(NT, WT, x) RUN_SAT_S_TRUNC_FMT_4(NT, WT, x)

#define RUN_SAT_S_TRUNC_FMT_5(NT, WT, x) sat_s_trunc_##WT##_to_##NT##_fmt_5 (x)
#define RUN_SAT_S_TRUNC_FMT_5_WRAP(NT, WT, x) RUN_SAT_S_TRUNC_FMT_5(NT, WT, x)

#define RUN_SAT_S_TRUNC_FMT_6(NT, WT, x) sat_s_trunc_##WT##_to_##NT##_fmt_6 (x)
#define RUN_SAT_S_TRUNC_FMT_6_WRAP(NT, WT, x) RUN_SAT_S_TRUNC_FMT_6(NT, WT, x)

#define RUN_SAT_S_TRUNC_FMT_7(NT, WT, x) sat_s_trunc_##WT##_to_##NT##_fmt_7 (x)
#define RUN_SAT_S_TRUNC_FMT_7_WRAP(NT, WT, x) RUN_SAT_S_TRUNC_FMT_7(NT, WT, x)

#define RUN_SAT_S_TRUNC_FMT_8(NT, WT, x) sat_s_trunc_##WT##_to_##NT##_fmt_8 (x)
#define RUN_SAT_S_TRUNC_FMT_8_WRAP(NT, WT, x) RUN_SAT_S_TRUNC_FMT_8(NT, WT, x)

#endif
