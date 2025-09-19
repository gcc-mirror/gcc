#include "perm.h"

#define SERIES_1(x, y) (x)
#define SERIES_2(x, y) (x), (x + 1)
#define SERIES_3(x, y) SERIES_1 (x, y), SERIES_2 (x + 1, y)
#define SERIES_4(x, y) SERIES_2 (x, y), SERIES_2 (x + 2, y)
#define SERIES_5(x, y) SERIES_2 (x, y), SERIES_3 (x + 2, y)
#define SERIES_6(x, y) SERIES_3 (x, y), SERIES_3 (x + 3, y)
#define SERIES_7(x, y) SERIES_3 (x, y), SERIES_4 (x + 3, y)
#define SERIES_8(x, y) SERIES_4 (x, y), SERIES_4 (x + 4, y)
#define SERIES_9(x, y) SERIES_4 (x, y), SERIES_5 (x + 4, y)
#define SERIES_10(x, y) SERIES_5 (x, y), SERIES_5 (x + 5, y)
#define SERIES_11(x, y) SERIES_5 (x, y), SERIES_6 (x + 5, y)
#define SERIES_12(x, y) SERIES_6 (x, y), SERIES_6 (x + 6, y)
#define SERIES_13(x, y) SERIES_6 (x, y), SERIES_7 (x + 6, y)
#define SERIES_14(x, y) SERIES_7 (x, y), SERIES_7 (x + 7, y)
#define SERIES_15(x, y) SERIES_7 (x, y), SERIES_8 (x + 7, y)
#define SERIES_16(x, y) SERIES_8 (x, y), SERIES_8 (x + 8, y)
#define SERIES_17(x, y) SERIES_8 (x, y), SERIES_9 (x + 8, y)
#define SERIES_18(x, y) SERIES_9 (x, y), SERIES_9 (x + 9, y)
#define SERIES_19(x, y) SERIES_9 (x, y), SERIES_10 (x + 9, y)
#define SERIES_20(x, y) SERIES_10 (x, y), SERIES_10 (x + 10, y)
#define SERIES_21(x, y) SERIES_10 (x, y), SERIES_11 (x + 10, y)
#define SERIES_22(x, y) SERIES_11 (x, y), SERIES_11 (x + 11, y)
#define SERIES_23(x, y) SERIES_11 (x, y), SERIES_12 (x + 11, y)
#define SERIES_24(x, y) SERIES_12 (x, y), SERIES_12 (x + 12, y)
#define SERIES_25(x, y) SERIES_12 (x, y), SERIES_13 (x + 12, y)
#define SERIES_26(x, y) SERIES_13 (x, y), SERIES_13 (x + 13, y)
#define SERIES_27(x, y) SERIES_13 (x, y), SERIES_14 (x + 13, y)
#define SERIES_28(x, y) SERIES_14 (x, y), SERIES_14 (x + 14, y)
#define SERIES_29(x, y) SERIES_14 (x, y), SERIES_15 (x + 14, y)
#define SERIES_30(x, y) SERIES_15 (x, y), SERIES_15 (x + 15, y)
#define SERIES_31(x, y) SERIES_15 (x, y), SERIES_16 (x + 15, y)
#define SERIES_32(x, y) SERIES_16 (x, y), SERIES_16 (x + 16, y)
#define SERIES_33(x, y) SERIES_16 (x, y), SERIES_17 (x + 16, y)
#define SERIES_34(x, y) SERIES_17 (x, y), SERIES_17 (x + 17, y)
#define SERIES_35(x, y) SERIES_17 (x, y), SERIES_18 (x + 17, y)
#define SERIES_36(x, y) SERIES_18 (x, y), SERIES_18 (x + 18, y)
#define SERIES_37(x, y) SERIES_18 (x, y), SERIES_19 (x + 18, y)
#define SERIES_38(x, y) SERIES_19 (x, y), SERIES_19 (x + 19, y)
#define SERIES_39(x, y) SERIES_19 (x, y), SERIES_20 (x + 19, y)
#define SERIES_40(x, y) SERIES_20 (x, y), SERIES_20 (x + 20, y)
#define SERIES_41(x, y) SERIES_20 (x, y), SERIES_21 (x + 20, y)
#define SERIES_42(x, y) SERIES_21 (x, y), SERIES_21 (x + 21, y)
#define SERIES_43(x, y) SERIES_21 (x, y), SERIES_22 (x + 21, y)
#define SERIES_44(x, y) SERIES_22 (x, y), SERIES_22 (x + 22, y)
#define SERIES_45(x, y) SERIES_22 (x, y), SERIES_23 (x + 22, y)
#define SERIES_46(x, y) SERIES_23 (x, y), SERIES_23 (x + 23, y)
#define SERIES_47(x, y) SERIES_23 (x, y), SERIES_24 (x + 23, y)
#define SERIES_48(x, y) SERIES_24 (x, y), SERIES_24 (x + 24, y)
#define SERIES_49(x, y) SERIES_24 (x, y), SERIES_25 (x + 24, y)
#define SERIES_50(x, y) SERIES_25 (x, y), SERIES_25 (x + 25, y)
#define SERIES_51(x, y) SERIES_25 (x, y), SERIES_26 (x + 25, y)
#define SERIES_52(x, y) SERIES_26 (x, y), SERIES_26 (x + 26, y)
#define SERIES_53(x, y) SERIES_26 (x, y), SERIES_27 (x + 26, y)
#define SERIES_54(x, y) SERIES_27 (x, y), SERIES_27 (x + 27, y)
#define SERIES_55(x, y) SERIES_27 (x, y), SERIES_28 (x + 27, y)
#define SERIES_56(x, y) SERIES_28 (x, y), SERIES_28 (x + 28, y)
#define SERIES_57(x, y) SERIES_28 (x, y), SERIES_29 (x + 28, y)
#define SERIES_58(x, y) SERIES_29 (x, y), SERIES_29 (x + 29, y)
#define SERIES_59(x, y) SERIES_29 (x, y), SERIES_30 (x + 29, y)
#define SERIES_60(x, y) SERIES_30 (x, y), SERIES_30 (x + 30, y)
#define SERIES_61(x, y) SERIES_30 (x, y), SERIES_31 (x + 30, y)
#define SERIES_62(x, y) SERIES_31 (x, y), SERIES_31 (x + 31, y)
#define SERIES_63(x, y) SERIES_31 (x, y), SERIES_32 (x + 31, y)
#define SERIES_64(x, y) SERIES_32 (x, y), SERIES_32 (x + 32, y)
#define SERIES_65(x, y) SERIES_32 (x, y), SERIES_33 (x + 32, y)
#define SERIES_66(x, y) SERIES_33 (x, y), SERIES_33 (x + 33, y)
#define SERIES_67(x, y) SERIES_33 (x, y), SERIES_34 (x + 33, y)
#define SERIES_68(x, y) SERIES_34 (x, y), SERIES_34 (x + 34, y)
#define SERIES_69(x, y) SERIES_34 (x, y), SERIES_35 (x + 34, y)
#define SERIES_70(x, y) SERIES_35 (x, y), SERIES_35 (x + 35, y)
#define SERIES_71(x, y) SERIES_35 (x, y), SERIES_36 (x + 35, y)
#define SERIES_72(x, y) SERIES_36 (x, y), SERIES_36 (x + 36, y)
#define SERIES_73(x, y) SERIES_36 (x, y), SERIES_37 (x + 36, y)
#define SERIES_74(x, y) SERIES_37 (x, y), SERIES_37 (x + 37, y)
#define SERIES_75(x, y) SERIES_37 (x, y), SERIES_38 (x + 37, y)
#define SERIES_76(x, y) SERIES_38 (x, y), SERIES_38 (x + 38, y)
#define SERIES_77(x, y) SERIES_38 (x, y), SERIES_39 (x + 38, y)
#define SERIES_78(x, y) SERIES_39 (x, y), SERIES_39 (x + 39, y)
#define SERIES_79(x, y) SERIES_39 (x, y), SERIES_40 (x + 39, y)
#define SERIES_80(x, y) SERIES_40 (x, y), SERIES_40 (x + 40, y)
#define SERIES_81(x, y) SERIES_40 (x, y), SERIES_41 (x + 40, y)
#define SERIES_82(x, y) SERIES_41 (x, y), SERIES_41 (x + 41, y)
#define SERIES_83(x, y) SERIES_41 (x, y), SERIES_42 (x + 41, y)
#define SERIES_84(x, y) SERIES_42 (x, y), SERIES_42 (x + 42, y)
#define SERIES_85(x, y) SERIES_42 (x, y), SERIES_43 (x + 42, y)
#define SERIES_86(x, y) SERIES_43 (x, y), SERIES_43 (x + 43, y)
#define SERIES_87(x, y) SERIES_43 (x, y), SERIES_44 (x + 43, y)
#define SERIES_88(x, y) SERIES_44 (x, y), SERIES_44 (x + 44, y)
#define SERIES_89(x, y) SERIES_44 (x, y), SERIES_45 (x + 44, y)
#define SERIES_90(x, y) SERIES_45 (x, y), SERIES_45 (x + 45, y)
#define SERIES_91(x, y) SERIES_45 (x, y), SERIES_46 (x + 45, y)
#define SERIES_92(x, y) SERIES_46 (x, y), SERIES_46 (x + 46, y)
#define SERIES_93(x, y) SERIES_46 (x, y), SERIES_47 (x + 46, y)
#define SERIES_94(x, y) SERIES_47 (x, y), SERIES_47 (x + 47, y)
#define SERIES_95(x, y) SERIES_47 (x, y), SERIES_48 (x + 47, y)
#define SERIES_96(x, y) SERIES_48 (x, y), SERIES_48 (x + 48, y)
#define SERIES_97(x, y) SERIES_48 (x, y), SERIES_49 (x + 48, y)
#define SERIES_98(x, y) SERIES_49 (x, y), SERIES_49 (x + 49, y)
#define SERIES_99(x, y) SERIES_49 (x, y), SERIES_50 (x + 49, y)
#define SERIES_100(x, y) SERIES_50 (x, y), SERIES_50 (x + 50, y)
#define SERIES_101(x, y) SERIES_50 (x, y), SERIES_51 (x + 50, y)
#define SERIES_102(x, y) SERIES_51 (x, y), SERIES_51 (x + 51, y)
#define SERIES_103(x, y) SERIES_51 (x, y), SERIES_52 (x + 51, y)
#define SERIES_104(x, y) SERIES_52 (x, y), SERIES_52 (x + 52, y)
#define SERIES_105(x, y) SERIES_52 (x, y), SERIES_53 (x + 52, y)
#define SERIES_106(x, y) SERIES_53 (x, y), SERIES_53 (x + 53, y)
#define SERIES_107(x, y) SERIES_53 (x, y), SERIES_54 (x + 53, y)
#define SERIES_108(x, y) SERIES_54 (x, y), SERIES_54 (x + 54, y)
#define SERIES_109(x, y) SERIES_54 (x, y), SERIES_55 (x + 54, y)
#define SERIES_110(x, y) SERIES_55 (x, y), SERIES_55 (x + 55, y)
#define SERIES_111(x, y) SERIES_55 (x, y), SERIES_56 (x + 55, y)
#define SERIES_112(x, y) SERIES_56 (x, y), SERIES_56 (x + 56, y)
#define SERIES_113(x, y) SERIES_56 (x, y), SERIES_57 (x + 56, y)
#define SERIES_114(x, y) SERIES_57 (x, y), SERIES_57 (x + 57, y)
#define SERIES_115(x, y) SERIES_57 (x, y), SERIES_58 (x + 57, y)
#define SERIES_116(x, y) SERIES_58 (x, y), SERIES_58 (x + 58, y)
#define SERIES_117(x, y) SERIES_58 (x, y), SERIES_59 (x + 58, y)
#define SERIES_118(x, y) SERIES_59 (x, y), SERIES_59 (x + 59, y)
#define SERIES_119(x, y) SERIES_59 (x, y), SERIES_60 (x + 59, y)
#define SERIES_120(x, y) SERIES_60 (x, y), SERIES_60 (x + 60, y)
#define SERIES_121(x, y) SERIES_60 (x, y), SERIES_61 (x + 60, y)
#define SERIES_122(x, y) SERIES_61 (x, y), SERIES_61 (x + 61, y)
#define SERIES_123(x, y) SERIES_61 (x, y), SERIES_62 (x + 61, y)
#define SERIES_124(x, y) SERIES_62 (x, y), SERIES_62 (x + 62, y)
#define SERIES_125(x, y) SERIES_62 (x, y), SERIES_63 (x + 62, y)
#define SERIES_126(x, y) SERIES_63 (x, y), SERIES_63 (x + 63, y)
#define SERIES_127(x, y) SERIES_63 (x, y), SERIES_64 (x + 63, y)
#define SERIES_128(x, y) SERIES_64 (x, y), SERIES_64 (x + 64, y)
#define SERIES_129(x, y) SERIES_64 (x, y), SERIES_65 (x + 64, y)
#define SERIES_128(x, y) SERIES_64 (x, y), SERIES_64 (x + 64, y)

#define PERMUTE4(TYPE, NUNITS, A, B, C)                                        \
  __attribute__ ((noipa)) void permute4_##A##_##B##_##C##_##TYPE               \
								(TYPE values1, \
								 TYPE values2, \
								 TYPE *out)    \
  {                                                                            \
    TYPE v = __builtin_shufflevector (values1, values2,                        \
				      MASK4_##NUNITS (0, NUNITS, A, B, C));    \
    *(TYPE *) out = v;                                                         \
  }

#define PERMUTE8(TYPE, NUNITS, A, B, C)                                        \
  __attribute__ ((noipa)) void permute8_##A##_##B##_##C##_##TYPE               \
								(TYPE values1, \
								 TYPE values2, \
								 TYPE *out)    \
  {                                                                            \
    TYPE v = __builtin_shufflevector (values1, values2,                        \
				      MASK8_##NUNITS (0, NUNITS, A, B, C));    \
    *(TYPE *) out = v;                                                         \
  }

#define PERMUTE16(TYPE, NUNITS, A, B, C)                                       \
  __attribute__ ((noipa)) void permute16_##A##_##B##_##C##_##TYPE              \
								(TYPE values1, \
								 TYPE values2, \
								 TYPE *out)    \
  {                                                                            \
    TYPE v = __builtin_shufflevector (values1, values2,                        \
				      MASK16_##NUNITS (0, NUNITS, A, B, C));   \
    *(TYPE *) out = v;                                                         \
  }

#define PERMUTE32(TYPE, NUNITS, A, B, C)                                       \
  __attribute__ ((noipa)) void permute32_##A##_##B##_##C##_##TYPE              \
								(TYPE values1, \
								 TYPE values2, \
								 TYPE *out)    \
  {                                                                            \
    TYPE v = __builtin_shufflevector (values1, values2,                        \
				      MASK32_##NUNITS (0, NUNITS, A, B, C));   \
    *(TYPE *) out = v;                                                         \
  }

#define PERMUTE64(TYPE, NUNITS, A, B, C)                                       \
  __attribute__ ((noipa)) void permute64_##A##_##B##_##C##_##TYPE              \
								(TYPE values1, \
								 TYPE values2, \
								 TYPE *out)    \
  {                                                                            \
    TYPE v = __builtin_shufflevector (values1, values2,                        \
				      MASK64_##NUNITS (0, NUNITS, A, B, C));   \
    *(TYPE *) out = v;                                                         \
  }

#define PERMUTE128(TYPE, NUNITS, A, B, C)                                      \
  __attribute__ ((noipa)) void permute128_##A##_##B##_##C##_##TYPE             \
								(TYPE values1, \
								 TYPE values2, \
								 TYPE *out)    \
  {                                                                            \
    TYPE v = __builtin_shufflevector (values1, values2,                        \
				      MASK128_##NUNITS (0, NUNITS, A, B, C));  \
    *(TYPE *) out = v;                                                         \
  }

#define TEST_128(FUNC, T) 							\
  T (vnx128qi, 128, FUNC)

#define TEST_64(FUNC, T) 							\
  T (vnx64qi, 64, FUNC)                                                         \
  T (vnx64hi, 64, FUNC)                                                         \
  TEST_128(FUNC, T)

#define TEST_32(FUNC, T)							\
  T (vnx32hi, 32, FUNC)                                                         \
  T (vnx32si, 32, FUNC)                                                         \
  T (vnx32sf, 32, FUNC)                                                         \
  T (vnx32qi, 32, FUNC)                                                         \
  TEST_64(FUNC, T)

#define TEST_16(FUNC, T)							\
  T (vnx16qi, 16, FUNC)                                                         \
  T (vnx16hi, 16, FUNC)                                                         \
  T (vnx16si, 16, FUNC)                                                         \
  T (vnx16di, 16, FUNC)                                                         \
  T (vnx16sf, 16, FUNC)                                                         \
  T (vnx16df, 16, FUNC) 							\
  TEST_32(FUNC, T)

#define TEST_8(FUNC, T)							        \
  T (vnx8qi, 8, FUNC)                                                           \
  T (vnx8hi, 8, FUNC)                                                           \
  T (vnx8si, 8, FUNC)                                                           \
  T (vnx8di, 8, FUNC)                                                           \
  T (vnx8sf, 8, FUNC)                                                           \
  T (vnx8df, 8, FUNC)                                                           \
  TEST_16(FUNC, T)

#define TEST_4(FUNC, T)                                                         \
  T (vnx4qi, 4, FUNC)                                                           \
  T (vnx4hi, 4, FUNC)                                                           \
  T (vnx4si, 4, FUNC)                                                           \
  T (vnx4di, 4, FUNC)                                                           \
  T (vnx4sf, 4, FUNC)                                                           \
  T (vnx4df, 4, FUNC)                                                           \
  TEST_8(FUNC, T)
