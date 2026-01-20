/* { dg-do compile } */
/* { dg-options "-O2 -mavx2" } */
typedef signed char int8_t;
typedef unsigned char uint8_t;
typedef unsigned short uint16_t;
typedef int int32_t;
typedef unsigned int uint32_t;
typedef long long int64_t;
typedef unsigned long long uint64_t;

#define BS_VEC(type, num) type __attribute__((vector_size(num * sizeof(type))))
inline int32_t backsmith_snippet_1141(BS_VEC(uint16_t, 32),
                                      BS_VEC(int32_t, 4) BS_ARG_1) {
  BS_ARG_1[0] = 2;
  BS_ARG_1 = __builtin_convertvector(
      __builtin_shufflevector(
          __builtin_shufflevector(
              (BS_VEC(uint8_t, 2)){},
              __builtin_convertvector(
                  __builtin_shufflevector(
                      BS_ARG_1, BS_ARG_1, 7, 3, 3, 7, 4, 2, 0, 3, 4, 2, 5, 0, 0,
                      1, 7, 7, 0, 2, 2, 3, 5, 4, 2, 6, 2, 3, 3, 7, 3, 1, 7, 3),
                  BS_VEC(uint8_t, 32)),
              0, 3, 3, 1, 2, 8, 1, 2, 5, 5, 0, 2, 0, 3, 6, 8),
          __builtin_shufflevector(
              (BS_VEC(uint8_t, 2)){},
              __builtin_convertvector(
                  __builtin_shufflevector(
                      BS_ARG_1, BS_ARG_1, 7, 3, 3, 7, 4, 2, 0, 3, 4, 2, 5, 0, 0,
                      1, 7, 7, 0, 2, 2, 3, 5, 4, 2, 6, 2, 3, 3, 7, 3, 1, 7, 3),
                  BS_VEC(uint8_t, 32)),
              0, 3, 3, 1, 2, 8, 1, 2, 5, 5, 0, 2, 0, 3, 6, 8),
          5, 9, 6, 6),
      BS_VEC(int32_t, 4));
  BS_ARG_1[4 ? BS_ARG_1[3] : 0] = 0;
  return BS_ARG_1[2];
}
int64_t backsmith_snippet_82(BS_VEC(uint64_t, 4), uint64_t, BS_VEC(uint32_t, 8),
                             BS_VEC(int8_t, 32));
uint64_t backsmith_pure_5(BS_VEC(uint32_t, 2) BS_ARG_0) {
  int64_t BS_VAR_2;
  uint32_t BS_VAR_3[8];
  BS_VEC(uint32_t, 32) BS_VAR_7;
  BS_VEC(int64_t, 4) BS_VAR_9;
  BS_VEC(int32_t, 4)
  BS_TEMP_132 = __builtin_convertvector(
      __builtin_shufflevector(BS_ARG_0, BS_ARG_0, 1, 0, 3, 0),
      BS_VEC(int32_t, 4));
  BS_VAR_3[8 ? backsmith_snippet_1141(
                   __builtin_convertvector(BS_VAR_7, BS_VEC(uint16_t, 32)),
                   (BS_VEC(int32_t, 4)){BS_TEMP_132[0], BS_TEMP_132[1],
                                        BS_TEMP_132[2], BS_TEMP_132[3]})
             : 0] |= 107245995;
  uint64_t BS_TEMP_134;
  uint32_t BS_TEMP_135 = __builtin_sub_overflow(
      0,
      backsmith_snippet_82(
          __builtin_convertvector(BS_VAR_9, BS_VEC(uint64_t, 4)), 0,
          __builtin_shufflevector(BS_VAR_7, BS_VAR_7, 8, 8, 4, 4, 5, 0, 5, 0),
          (BS_VEC(int8_t, 32)){}),
      &BS_TEMP_134);
  uint32_t BS_TEMP_136 = BS_TEMP_135 ? BS_VAR_3[0] : 0;
  uint32_t BS_TEMP_137 = BS_TEMP_136;
  for (; BS_TEMP_137;)
    ;
}
