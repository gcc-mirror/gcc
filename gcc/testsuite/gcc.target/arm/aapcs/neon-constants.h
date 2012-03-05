

#include "arm_neon.h"

const int32x4_t i32x4_constvec1 = { 1101, 1102, 1103, 1104};
const int32x4_t i32x4_constvec2 = { 2101, 2102, 2103, 2104};

#define ELEM(INDEX) .val[INDEX]

const int32x4x2_t i32x4x2_constvec1 =   {ELEM(0) = {0xaddebccb,11,12,13}, 
					 ELEM(1) = {14, 15, 16, 17} };

const int32x4x2_t i32x4x2_constvec2 = { ELEM(0) = {0xaadebcca,11,12,13}, 
			                ELEM(1) = {140, 15, 16, 17}};

const int32x4x3_t i32x4x3_constvec1 = { ELEM(0) = {0xabbccdde,8, 9, 10},
					ELEM(1) = {0xabcccdde, 26, 27, 28},
			                ELEM(2) = {0xaccccddf, 29, 30, 31}};

const int32x4x3_t i32x4x3_constvec2 = { ELEM(0) = {0xbccccdd0,8, 9, 10},
					ELEM(1) = {0xbdfe1000, 26, 27, 28},
			                ELEM(2) = {0xaccccddf, 29, 30, 31}};
const float32x4x2_t f32x4x2_constvec1 =
  { ELEM(0) = { 7.101f, 0.201f, 0.301f, 0.401f} ,
    ELEM(1) = { 8.101f, 0.501f, 0.601f, 0.701f} };

const float32x4x2_t f32x4x2_constvec2 = 
  { ELEM(0) = { 11.99f , 11.21f, 1.27f, 8.74f},
    ELEM(1) = { 13.45f , 1.23f ,1.24f, 1.26f}};

const int32x2_t i32x2_constvec1 = { 1283, 1345 };
const int32x2x2_t i32x2x2_constvec1 = { ELEM(0) = { 0xabcdefab, 32 },
					ELEM(1) = { 0xabcdefbc, 33 }};
