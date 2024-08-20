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

#define RUN_SAT_U_ADD_IMM_FMT_1(T, x, IMM, expect) \
  if (sat_u_add_imm##IMM##_##T##_fmt_1(x) != expect) __builtin_abort ()

#define RUN_SAT_U_ADD_IMM_FMT_2(T, x, IMM, expect) \
  if (sat_u_add_imm##IMM##_##T##_fmt_2(x) != expect) __builtin_abort ()

#define RUN_SAT_U_ADD_IMM_FMT_3(T, x, IMM, expect) \
  if (sat_u_add_imm##IMM##_##T##_fmt_3(x) != expect) __builtin_abort ()

#define RUN_SAT_U_ADD_IMM_FMT_4(T, x, IMM, expect) \
  if (sat_u_add_imm##IMM##_##T##_fmt_4(x) != expect) __builtin_abort ()

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

#define RUN_SAT_U_TRUNC_FMT_1(NT, WT, x) sat_u_trunc_##WT##_to_##NT##_fmt_1 (x)
#define RUN_SAT_U_TRUNC_FMT_1_WRAP(NT, WT, x) RUN_SAT_U_TRUNC_FMT_1(NT, WT, x)

#define RUN_SAT_U_TRUNC_FMT_2(NT, WT, x) sat_u_trunc_##WT##_to_##NT##_fmt_2 (x)
#define RUN_SAT_U_TRUNC_FMT_2_WRAP(NT, WT, x) RUN_SAT_U_TRUNC_FMT_2(NT, WT, x)

#define RUN_SAT_U_TRUNC_FMT_3(NT, WT, x) sat_u_trunc_##WT##_to_##NT##_fmt_3 (x)
#define RUN_SAT_U_TRUNC_FMT_3_WRAP(NT, WT, x) RUN_SAT_U_TRUNC_FMT_3(NT, WT, x)

#endif
