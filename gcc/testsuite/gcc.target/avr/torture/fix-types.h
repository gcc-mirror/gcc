typedef __INT8_TYPE__  int_hr_t;
typedef __UINT8_TYPE__ int_uhr_t;

typedef __INT16_TYPE__  int_hk_t;
typedef __UINT16_TYPE__ int_uhk_t;
typedef __INT16_TYPE__  int_r_t;
typedef __UINT16_TYPE__ int_ur_t;

typedef __INT32_TYPE__  int_k_t;
typedef __UINT32_TYPE__ int_uk_t;
typedef __INT32_TYPE__  int_lr_t;
typedef __UINT32_TYPE__ int_ulr_t;

typedef __INT64_TYPE__  int_lk_t;
typedef __UINT64_TYPE__ int_ulk_t;
typedef __INT64_TYPE__  int_llr_t;
typedef __UINT64_TYPE__ int_ullr_t;
typedef __INT64_TYPE__  int_llk_t;
typedef __UINT64_TYPE__ int_ullk_t;

typedef __INT16_TYPE__  xint_hr_t;
typedef __UINT16_TYPE__ xint_uhr_t;

typedef __INT32_TYPE__  xint_hk_t;
typedef __UINT32_TYPE__ xint_uhk_t;
typedef __INT32_TYPE__  xint_r_t;
typedef __UINT32_TYPE__ xint_ur_t;

typedef __INT64_TYPE__  xint_k_t;
typedef __UINT64_TYPE__ xint_uk_t;
typedef __INT64_TYPE__  xint_lr_t;
typedef __UINT64_TYPE__ xint_ulr_t;

#define INThr_MAX __INT8_MAX__
#define INThr_MIN (-__INT8_MAX__-1)
#define INTuhr_MAX __UINT8_MAX__

#define INTr_MAX __INT16_MAX__
#define INTr_MIN (-__INT16_MAX__-1)
#define INTur_MAX __UINT16_MAX__

#define INThk_MAX __INT16_MAX__
#define INThk_MIN (-__INT16_MAX__-1)
#define INTuhk_MAX __UINT16_MAX__

#define INTlr_MAX __INT32_MAX__
#define INTlr_MIN (-__INT32_MAX__-1)
#define INTulr_MAX __UINT32_MAX__

#define INTk_MAX __INT32_MAX__
#define INTk_MIN (-__INT32_MAX__-1)
#define INTuk_MAX __UINT32_MAX__

#define INTlk_MAX __INT64_MAX__
#define INTlk_MIN (-__INT64_MAX__-1)
#define INTulk_MAX __UINT64_MAX__

#define INTllk_MAX __INT64_MAX__
#define INTllk_MIN (-__INT64_MAX__-1)
#define INTullk_MAX __UINT64_MAX__

#define SS_FUN(NAME, OP, T, FX)                 \
  T __attribute__((noinline,noclone))           \
  NAME##_##FX (T fa, T fb)                      \
  {                                             \
    int_##FX##_t ia;                            \
    int_##FX##_t ib;                            \
    xint_##FX##_t ic;                           \
    __builtin_memcpy (&ia, &fa, sizeof (ia));   \
    __builtin_memcpy (&ib, &fb, sizeof (ib));   \
    ic = (xint_##FX##_t) ia OP ib;              \
    if (ic > INT##FX##_MAX)                     \
      ic = INT##FX##_MAX;                       \
    else if (ic < INT##FX##_MIN)                \
      ic = INT##FX##_MIN;                       \
    ia = (int_##FX##_t) ic;                     \
    __builtin_memcpy (&fa, &ia, sizeof (ia));   \
    return fa;                                  \
  }

#define US_FUN(NAME, OP, T, FX)                 \
  T __attribute__((noinline,noclone))           \
  NAME##_##FX (T fa, T fb)                      \
  {                                             \
    int_##FX##_t ia;                            \
    int_##FX##_t ib;                            \
    xint_##FX##_t ic;                           \
    __builtin_memcpy (&ia, &fa, sizeof (ia));   \
    __builtin_memcpy (&ib, &fb, sizeof (ib));   \
    ic = (xint_##FX##_t) ia OP ib;              \
    if (ic > INT##FX##_MAX)                     \
      ic = INT##FX##_MAX;                       \
    else if (ic < 0)                            \
      ic = 0;                                   \
    ia = (int_##FX##_t) ic;                     \
    __builtin_memcpy (&fa, &ia, sizeof (ia));   \
    return fa;                                  \
  }

#define SS_LFUN(NAME, OP, T, FX, CMP)           \
  T __attribute__((noinline,noclone))           \
  NAME##_##FX (T fa, T fb)                      \
  {                                             \
    int_##FX##_t ia;                            \
    int_##FX##_t ib;                            \
    int_##FX##_t ic;                            \
    __builtin_memcpy (&ia, &fa, sizeof (ia));   \
    __builtin_memcpy (&ib, &fb, sizeof (ib));   \
    ic = (int_##FX##_t) ia OP ib;               \
    if (ic < ia && ib CMP 0)                    \
      ic = INT##FX##_MAX;                       \
    else if (ic > ia && 0 CMP ib)               \
      ic = INT##FX##_MIN;                       \
    __builtin_memcpy (&fa, &ic, sizeof (ic));   \
    return fa;                                  \
  }

#define US_LFUN(NAME, OP, T, FX, CMP)           \
  T __attribute__((noinline,noclone))           \
  NAME##_##FX (T fa, T fb)                      \
  {                                             \
    int_##FX##_t ia;                            \
    int_##FX##_t ib;                            \
    int_##FX##_t ic;                            \
    __builtin_memcpy (&ia, &fa, sizeof (ia));   \
    __builtin_memcpy (&ib, &fb, sizeof (ib));   \
    ic = (int_##FX##_t) ia OP ib;               \
    if (ia CMP ic && 1 CMP 0)                   \
      ic = INT##FX##_MAX;                       \
    if (ia CMP ic && 0 CMP 1)                   \
      ic = 0;                                   \
    __builtin_memcpy (&fa, &ic, sizeof (ic));   \
    return fa;                                  \
  }
