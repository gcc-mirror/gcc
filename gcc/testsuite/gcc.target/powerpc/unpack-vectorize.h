typedef signed long long sll;
typedef unsigned long long ull;
typedef signed int si;
typedef unsigned int ui;
typedef signed short sh;
typedef unsigned short uh;
typedef signed char sc;
typedef unsigned char uc;

#ifndef ALIGN
#define ALIGN 32
#endif

#define ALIGN_ATTR __attribute__((__aligned__(ALIGN)))

#define N 128

#define DEF_ARR(TYPE)                                                         \
  TYPE TYPE##_a[N] ALIGN_ATTR;                                                \
  TYPE TYPE##_b[N] ALIGN_ATTR;                                                \
  TYPE TYPE##_c[N] ALIGN_ATTR;

#define TEST1(NTYPE, WTYPE)                                                    \
  __attribute__((noipa)) void test1_##NTYPE##_##WTYPE() {                      \
    for (int i = 0; i < N; i++)                                                \
      WTYPE##_c[i] = NTYPE##_a[i] + NTYPE##_b[i];                              \
  }

#define CHECK1(NTYPE, WTYPE)                                                   \
  __attribute__((noipa, optimize(0))) void check1_##NTYPE##_##WTYPE() {        \
    for (int i = 0; i < N; i++) {                                              \
      NTYPE##_a[i] = 2 * i * sizeof(NTYPE) + 10;                               \
      NTYPE##_b[i] = 7 * i * sizeof(NTYPE) / 5 - 10;                           \
    }                                                                          \
    test1_##NTYPE##_##WTYPE();                                                 \
    for (int i = 0; i < N; i++) {                                              \
      WTYPE exp = NTYPE##_a[i] + NTYPE##_b[i];                                 \
      if (WTYPE##_c[i] != exp)                                                 \
        __builtin_abort();                                                     \
    }                                                                          \
  }

