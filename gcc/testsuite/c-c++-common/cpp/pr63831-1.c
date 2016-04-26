/* PR preprocessor/63831 */
/* { dg-do compile } */

#ifdef __has_attribute
typedef char T1[__has_attribute (__noreturn__) ? 1 : -1];
typedef char T2[__has_attribute (alloc_size) == 1 ? 1 : -1];
typedef char T3[__has_attribute (non_existent_attribuuuute) == 0 ? 1 : -1];
#endif
#if __has_attribute (noreturn)
typedef char T4;
#endif
#define d deprecated
typedef char T5[__has_attribute (d) ? 1 : -1];
T1 t1;
T2 t2;
T3 t3;
T4 t4;
T5 t5;
#ifdef __cplusplus
typedef char T6[__has_attribute (gnu::__noreturn__) ? 1 : -1];
typedef char T7[__has_attribute (gnu::alloc_size) == 1 ? 1 : -1];
typedef char T8[__has_attribute (gnu::non_existent_attribuuuute) == 0 ? 1 : -1];
#if __has_attribute (gnu::noreturn)
typedef char T9;
#endif
#define d2 gnu::deprecated
typedef char T10[__has_attribute (d) ? 1 : -1];
T6 t6;
T7 t7;
T8 t8;
T9 t9;
T10 t10;
#endif
#ifdef __has_cpp_attribute
typedef char T11[__has_cpp_attribute (__noreturn__) ? 1 : -1];
typedef char T12[__has_cpp_attribute (alloc_size) == 1 ? 1 : -1];
typedef char T13[__has_cpp_attribute (non_existent_attribuuuute) == 0 ? 1 : -1];
#endif
#if __has_cpp_attribute (noreturn)
typedef char T14;
#endif
#define d deprecated
typedef char T15[__has_cpp_attribute (d) ? 1 : -1];
T11 t11;
T12 t12;
T13 t13;
T14 t14;
T15 t15;
#ifdef __cplusplus
typedef char T16[__has_cpp_attribute (gnu::__noreturn__) ? 1 : -1];
typedef char T17[__has_cpp_attribute (gnu::alloc_size) == 1 ? 1 : -1];
typedef char T18[__has_cpp_attribute (gnu::non_existent_attribuuuute) == 0 ? 1 : -1];
#if __has_cpp_attribute (gnu::noreturn)
typedef char T19;
#endif
#define d2 gnu::deprecated
typedef char T20[__has_cpp_attribute (d) ? 1 : -1];
T16 t16;
T17 t17;
T18 t18;
T19 t19;
T20 t20;
#endif
int t21 = __has_attribute (noreturn) + __has_cpp_attribute (__malloc__);
