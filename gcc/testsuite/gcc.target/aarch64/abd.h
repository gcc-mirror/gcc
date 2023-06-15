#ifdef ABD_IDIOM

#define TEST1(S, TYPE)				\
__attribute__((noipa))				\
void fn_##S##_##TYPE (S TYPE * restrict a,	\
		      S TYPE * restrict b,	\
		      S TYPE * restrict out) {	\
  for (int i = 0; i < N; i++) {			\
    signed TYPE diff = b[i] - a[i];		\
    out[i] = diff > 0 ? diff : -diff;		\
} }

#define TEST2(S, TYPE1, TYPE2)			\
__attribute__((noipa))				\
void fn_##S##_##TYPE1##_##TYPE1##_##TYPE2	\
    (S TYPE1 * restrict a,			\
     S TYPE1 * restrict b,			\
     S TYPE2 * restrict out) {			\
  for (int i = 0; i < N; i++) {			\
    signed TYPE2 diff = b[i] - a[i];		\
    out[i] = diff > 0 ? diff : -diff;		\
} }

#define TEST3(S, TYPE1, TYPE2, TYPE3)		\
__attribute__((noipa))				\
void fn_##S##_##TYPE1##_##TYPE2##_##TYPE3	\
    (S TYPE1 * restrict a,			\
     S TYPE2 * restrict b,			\
     S TYPE3 * restrict out) {			\
  for (int i = 0; i < N; i++) {			\
    signed TYPE3 diff = b[i] - a[i];		\
    out[i] = diff > 0 ? diff : -diff;		\
} }

#endif

#ifdef ABD_ABS

#define TEST1(S, TYPE)				\
__attribute__((noipa))				\
void fn_##S##_##TYPE (S TYPE * restrict a,	\
		      S TYPE * restrict b,	\
		      S TYPE * restrict out) {	\
  for (int i = 0; i < N; i++)			\
    out[i] = __builtin_abs(a[i] - b[i]);	\
}

#define TEST2(S, TYPE1, TYPE2)			\
__attribute__((noipa))				\
void fn_##S##_##TYPE1##_##TYPE1##_##TYPE2	\
    (S TYPE1 * restrict a,			\
     S TYPE1 * restrict b,			\
     S TYPE2 * restrict out) {			\
  for (int i = 0; i < N; i++)			\
    out[i] = __builtin_abs(a[i] - b[i]);	\
}

#define TEST3(S, TYPE1, TYPE2, TYPE3)		\
__attribute__((noipa))				\
void fn_##S##_##TYPE1##_##TYPE2##_##TYPE3	\
    (S TYPE1 * restrict a,			\
     S TYPE2 * restrict b,			\
     S TYPE3 * restrict out) {			\
  for (int i = 0; i < N; i++)			\
    out[i] = __builtin_abs(a[i] - b[i]);	\
}

#endif
