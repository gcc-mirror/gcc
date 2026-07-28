// !!! Requires the fx64 <-> float conversions from AVR-LibC.

#include <stdfix.h>
#include <stdbool.h>
#include <stdlib.h>
#include <avr/pgmspace.h>

#define NI __attribute((noipa))
#define ARRAY_SIZE(x) (sizeof(x) / sizeof(*x))

typedef long accum lk_t;
typedef long long accum llk_t;
typedef long long fract llr_t;

typedef unsigned long accum ulk_t;
typedef unsigned long long accum ullk_t;
typedef unsigned long long fract ullr_t;

// Values are in fmin <= x < fmax.
#define fmax_llr 1.0f
#define fmin_llr (-fmax_llr)
#define fmax_ullr fmax_llr
#define fmin_ullr 0.0f

#define fmax_lk 0x1.0p32f
#define fmin_lk (-fmax_lk)
#define fmax_ulk fmax_lk
#define fmin_ulk 0.0f

#define fmax_llk 0x1.0p16f
#define fmin_llk (-fmax_llk)
#define fmax_ullk fmax_llk
#define fmin_ullk 0.0f

#define UMAX 0xffffffffffffffff
#define SMAX 0x7fffffffffffffff
#define SMIN 0x8000000000000000
#define X80  0x8000000000000000

// Values are in min <= x <= max.
#define max_lk lkbits (SMAX)
#define min_lk lkbits (SMIN)
#define max_ulk ulkbits (UMAX)
#define min_ulk ulkbits (0)

#define max_llk llkbits (SMAX)
#define min_llk llkbits (SMIN)
#define max_ullk ullkbits (UMAX)
#define min_ullk ullkbits (0)

#define max_llr llrbits (SMAX)
#define min_llr llrbits (SMIN)
#define max_ullr ullrbits (UMAX)
#define min_ullr ullrbits (0)

#define lk_1   (1ull << __DA_FBIT__)
#define ulk_1  (1ull << __UDA_FBIT__)
#define llk_1  (1ull << __TA_FBIT__)
#define ullk_1 (1ull << __UTA_FBIT__)

#define id_lk   10
#define id_ulk  20
#define id_llk  30
#define id_ullk 40
#define id_llr  50
#define id_ullr 60
