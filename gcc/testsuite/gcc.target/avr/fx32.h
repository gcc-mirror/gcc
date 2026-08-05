#include <stdfix.h>
#include <stdbool.h>
#include <stdlib.h>
#include <avr/pgmspace.h>

#define NI __attribute((noipa))
#define ARRAY_SIZE(x) (sizeof(x) / sizeof(*x))

typedef accum k_t;
typedef unsigned accum uk_t;

typedef sat accum sat_k_t;
typedef sat unsigned accum sat_uk_t;

// Values are in fmin <= x < fmax.
#define fmax_k 0x1.0p16f
#define fmin_k (-fmax_k)
#define fmax_uk fmax_k
#define fmin_uk 0.0f

#define UMAX 0xffffffff
#define SMAX 0x7fffffff
#define SMIN 0x80000000
#define X80  0x80000000

// Values are in min <= x <= max.
#define max_k kbits (SMAX)
#define min_k kbits (SMIN)
#define max_uk ukbits (UMAX)
#define min_uk ukbits (0)

#define k_1   (1ul << __SA_FBIT__)
#define uk_1  (1ul << __USA_FBIT__)

#define id_k   10
#define id_uk  20
