#ifndef FX_H
#define FX_H

#include <stdfix.h>
#include <stdbool.h>
#include <stdlib.h>
#include <avr/pgmspace.h>

#define NI __attribute((noipa))
#define ARRAY_SIZE(x) (sizeof(x) / sizeof(*x))

typedef short fract hr_t;
typedef unsigned short fract uhr_t;
typedef short accum hk_t;
typedef unsigned short accum uhk_t;
typedef fract r_t;
typedef unsigned fract ur_t;

typedef sat short fract sat_hr_t;
typedef sat unsigned short fract sat_uhr_t;
typedef sat short accum sat_hk_t;
typedef sat short unsigned accum sat_uhk_t;
typedef sat fract sat_r_t;
typedef sat unsigned fract sat_ur_t;

typedef accum k_t;
typedef unsigned accum uk_t;
typedef long fract lr_t;
typedef long unsigned fract ulr_t;

typedef sat accum sat_k_t;
typedef sat unsigned accum sat_uk_t;
typedef sat long fract sat_lr_t;
typedef sat long unsigned fract sat_ulr_t;

typedef long accum lk_t;
typedef unsigned long accum ulk_t;
typedef long long accum llk_t;
typedef unsigned long long accum ullk_t;
typedef long long fract llr_t;
typedef long long unsigned fract ullr_t;

typedef sat long accum sat_lk_t;
typedef sat unsigned long accum sat_ulk_t;
typedef sat long long accum sat_llk_t;
typedef sat unsigned long long accum sat_ullk_t;
typedef sat long long fract sat_llr_t;
typedef sat long long unsigned fract sat_ullr_t;

#define hk_1   (1u << __HA_FBIT__)
#define uhk_1  (1u << __UHA_FBIT__)

#define k_1   (1ul << __SA_FBIT__)
#define uk_1  (1ul << __USA_FBIT__)

#endif /* FX_H */
