#include <stdfix.h>
#include <stdbool.h>
#include <stdlib.h>
#include <avr/pgmspace.h>

#define NI __attribute((noipa))
#define ARRAY_SIZE(x) (sizeof(x) / sizeof(*x))

typedef short accum hk_t;
typedef unsigned short accum uhk_t;
typedef fract r_t;
typedef unsigned fract ur_t;

typedef short sat accum sat_hk_t;
typedef short sat unsigned accum sat_uhk_t;
typedef sat fract sat_r_t;
typedef sat unsigned fract sat_ur_t;

// Values are in fmin <= x < fmax.
#define fmax_hk 0x1.0p8f
#define fmin_hk (-fmax_hk)
#define fmax_uhk fmax_hk
#define fmin_uhk 0.0f
#define fmax_r  0x1.0p0f
#define fmin_r  (-fmax_r)
#define fmax_ur fmax_r
#define fmin_ur 0.0f

#define UMAX 0xffff
#define SMAX 0x7fff
#define SMIN 0x8000
#define X80  0x8000

// Values are in min <= x <= max.
#define max_hk hkbits (SMAX)
#define min_hk hkbits (SMIN)
#define max_uhk uhkbits (UMAX)
#define min_uhk uhkbits (0)
#define max_r   rbits (SMAX)
#define min_r   rbits (SMIN)
#define max_ur  urbits (UMAX)
#define min_ur  urbits (0)

#define hk_1   (1u << __HA_FBIT__)
#define uhk_1  (1u << __UHA_FBIT__)

#define id_hk   10
#define id_uhk  20
#define id_r    30
#define id_ur   40
