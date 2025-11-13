/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -w -O0" { target rv64 } } */
/* { dg-options "-march=rv32gcv -mabi=ilp32 -w -O0" { target rv32 } } */


typedef signed char int8_t;
typedef short int16_t;
typedef int int32_t;
typedef long int64_t;
typedef char uint8_t;
typedef unsigned short uint16_t;
typedef unsigned long uint64_t;
uint64_t BS_CHECKSUM_ARR_7;
struct S0 {
  unsigned f0 : 9;
  signed : 26
} * g_108, g_148, g_150;
int32_t g_8[];
int32_t g_44, g_130_0;
uint16_t g_76_0;
int64_t g_158[];
int64_t g_158_1_0_3;
uint8_t *func_56_l_165;
__attribute__((always_inline)) int32_t
backsmith_snippet_544(int16_t __attribute__((vector_size(32 * sizeof(int16_t))))
                      BS_ARG_0) {
  BS_ARG_0 = BS_ARG_0 =
      (int16_t __attribute__((vector_size(32 * sizeof(int16_t))))) -
      __builtin_convertvector(
          (int16_t __attribute__((vector_size(32 *sizeof(int16_t))))){
              ~BS_ARG_0[3], ~BS_ARG_0[3], ~BS_ARG_0[3], ~BS_ARG_0[3],
              ~BS_ARG_0[3], ~BS_ARG_0[3], ~BS_ARG_0[3], ~BS_ARG_0[3],
              ~BS_ARG_0[3], ~BS_ARG_0[3], ~BS_ARG_0[3], ~BS_ARG_0[3],
              ~BS_ARG_0[3], ~BS_ARG_0[3], ~BS_ARG_0[3], ~BS_ARG_0[3],
              ~BS_ARG_0[3], ~BS_ARG_0[3], ~BS_ARG_0[3], ~BS_ARG_0[3],
              ~BS_ARG_0[3], ~BS_ARG_0[3], ~BS_ARG_0[3], ~BS_ARG_0[3],
              ~BS_ARG_0[3], ~BS_ARG_0[3], ~BS_ARG_0[3], ~BS_ARG_0[3],
              ~BS_ARG_0[3], ~BS_ARG_0[3], ~BS_ARG_0[3], ~BS_ARG_0[3]},
          uint16_t __attribute__((vector_size(32 * sizeof(uint16_t)))));
  return ((union {
           int16_t __attribute__((vector_size(2 * sizeof(int16_t)))) src;
           int32_t dst
         })__builtin_shufflevector(BS_ARG_0, BS_ARG_0, 43, 7))
      .dst;
}
int8_t(safe_add_func_int8_t_s_s)(int8_t);
uint8_t(safe_lshift_func_uint8_t_u_s)(uint8_t);
unsigned safe_mul_func_uint32_t_u_u(int, int);
uint8_t func_56(uint64_t p_57, int32_t *, int8_t p_59, uint16_t p_60,
                int32_t *p_61) {
  int64_t __attribute__((vector_size(8 * sizeof(int64_t))))
  BS_VAR_0 = {30330355581};
  int8_t __attribute__((vector_size(2))) BS_VAR_1 = {7};
  int64_t __attribute__((vector_size(32 * sizeof(int64_t)))) BS_VAR_2[33] = {};
  int64_t BS_VAR_2_3_0;
  int16_t __attribute__((vector_size(32 * sizeof(int16_t)))) BS_VAR_3 = {};
  int volatile BS_COND_18 = BS_COND_18, BS_COND_19 = 2046083389,
               BS_COND_20 = BS_COND_20;
  int64_t __attribute__((vector_size(32 * sizeof(int64_t)))) BS_VAR_5 = {};
  int16_t __attribute__((vector_size(sizeof(int16_t))))
  BS_VAR_6 = {90173011178118114616913331460};
  int64_t __attribute__((vector_size(16))) BS_VAR_7 = {20385270170872471};
  switch (BS_COND_19)
  case 4044:
  case 6463:
  case 7825:
  case 40964:
  case 71504:
  case 50913903:
  case 3624:
  case 3379:
  case 80451787:
  case 4004383:
  case 4133:
  case 2098801:
  case 2000071388:
  case 400611:
  case 4155:
  case 6888:
  case 3506:
  case 9734:
  case 100094276:
  case 3121:
  case 9659:
  case 5892:
  case 9507:
  case 1067596166:
  case 2099438381:
  case 2406:
  case 5442:
  case 60746:
  case 31236:
  case 3574:
  case 42025:
  case 3315:
  case 2096976873:
  case 6082416:
  case 909667:
  case 5535:
  case 2554:
  case 31179:
  case 2046083389:
  case 7309:
  case 4706:
  case 6816:
  case 7189:
  case 3402:
  case 706316781:
  case 3570:
  case 4376:
  case 9698:
  case 4190:
  case 6195:
  case 6714:
  case 7540:
  case 2145:
  case 61108:
  case 2442:
  case 1049158:
  case 60055:
  case 4302:
    break;
  int64_t __attribute__((vector_size(2 * sizeof(int64_t))))
  BS_VAR_8[] = {52623717371,
                908827026188589,
                3253451771131199467,
                3253451771131199467,
                8379,
                1,
                500143520836,
                0,
                5884817202404184392,
                5884817202404184392,
                (int64_t __attribute__((vector_size(2 * sizeof(int64_t))))){
                    8741271462101646089, 8741271462101646089}};
  int BS_VAR_9 = 2177;
  int64_t __attribute__((vector_size(32 * sizeof(int64_t)))) BS_VAR_10[37] = {};
  int16_t __attribute__((vector_size(8 * sizeof(int16_t)))) BS_VAR_11[5] = {};
  uint8_t __attribute__((vector_size(2))) BS_VAR_12 = {4};
  uint64_t LOCAL_CHECKSUM = BS_CHECKSUM_ARR_7;
  struct S0 **l_160 = &g_108;
  int32_t l_161 = 3842;
  int64_t *l_162[] = {&g_158_1_0_3, &g_158_1_0_3, &g_158_1_0_3, &g_158_1_0_3};
  struct S0 ***l_176 = &l_160;
  int32_t l_181 = g_44;
  int32_t *l_180 = &l_181;
  *func_56_l_165 = p_60;
  l_162[7] == g_158 != safe_lshift_func_uint8_t_u_s(*func_56_l_165);
  g_130_0 |= l_160 != (g_148.f0 & 1 < l_161 >= ((g_76_0 = 0) < g_8[5]), l_160);
  if (g_148.f0)
    BS_VAR_8[p_57 < 7 ? p_57 : 0] = __builtin_shufflevector(
        __builtin_shufflevector(BS_VAR_8[0], BS_VAR_8[0], 1, 0, 1, 2),
        __builtin_convertvector(
            (uint64_t __attribute__((vector_size(4 *sizeof(uint64_t))))){
                p_57, p_57, p_57, p_57},
            int64_t __attribute__((vector_size(4 * sizeof(int64_t))))),
        6, 0);
  l_161 = safe_add_func_int8_t_s_s(l_161);
  for (; l_161 != 20;)
    p_57 = 9;
  p_57 = safe_add_func_int8_t_s_s(p_57);
  for (; p_57 > 2;) {
    int BS_TEMP_89 = 7760,
        BS_TEMP_90 = backsmith_snippet_544(__builtin_convertvector(
            __builtin_shufflevector(BS_VAR_8[4], BS_VAR_8[4], 3, 2, 1, 2, 2, 2,
                                    0, 0, 0, 2, 3, 0, 2, 0, 1, 3, 3, 1, 1, 2, 2,
                                    0, 2, 3, 3, 0, 1, 3, 0, 1, 3, 3),
            int16_t __attribute__((vector_size(32 * sizeof(int16_t))))));
    BS_VAR_2[__builtin_bswap32(BS_TEMP_89 < 2 ? (unsigned)l_161 << BS_TEMP_89
                                              : BS_VAR_2_3_0) < 3
                 ? __builtin_bswap32(BS_TEMP_89 < 2
                                         ? (unsigned)l_161 << BS_TEMP_89
                                         : BS_VAR_2_3_0)
                 : 0] =
        __builtin_convertvector(
            (int32_t __attribute__((vector_size(32 *sizeof(int32_t))))){
                BS_TEMP_90, BS_TEMP_90, BS_TEMP_90, BS_TEMP_90, BS_TEMP_90,
                BS_TEMP_90, BS_TEMP_90, BS_TEMP_90, BS_TEMP_90, BS_TEMP_90,
                BS_TEMP_90, BS_TEMP_90, BS_TEMP_90, BS_TEMP_90, BS_TEMP_90,
                BS_TEMP_90, BS_TEMP_90, BS_TEMP_90, BS_TEMP_90, BS_TEMP_90,
                BS_TEMP_90, BS_TEMP_90, BS_TEMP_90, BS_TEMP_90, BS_TEMP_90,
                BS_TEMP_90, BS_TEMP_90, BS_TEMP_90, BS_TEMP_90, BS_TEMP_90,
                BS_TEMP_90, BS_TEMP_90},
            int64_t __attribute__((vector_size(32 * sizeof(int64_t)))));
  }
  *p_61 &= l_161 >=
           safe_mul_func_uint32_t_u_u(
               g_150.f0, ((*l_176 = l_160) != g_108) > l_160 == 0 == g_148.f0);
  *l_180 = 0;
  BS_VAR_3 = __builtin_shufflevector(
      BS_VAR_11[p_59 < (uint64_t)5 ? (uint64_t)p_59 : 0],
      BS_VAR_11[p_59 < (uint64_t)5 ? (uint64_t)p_59 : 0], 0, 4, 5, 4, 0, 3, 3,
      2, 5, 4, 3, 0, 4, 3, 9, 1, 8, 8, 4, 3, 4, 0, 1, 9, 0, 0, 0, 1, 5, 2, 0,
      9);
  return p_57;
}
