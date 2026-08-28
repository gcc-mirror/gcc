#ifndef HAVE_DEFINED_GROUP_OVERLAP_H
#define HAVE_DEFINED_GROUP_OVERLAP_H

#include <stdint.h>
#include <stdbool.h>
#include <riscv_vector.h>

#define LOOP_UNARY_BODY_X4(NT, WT, LD_F, OUT_F, ST_F, OUT, START, VL)  \
    NT vs0 = LD_F ((void *)START, VL); START += VL;                    \
    NT vs1 = LD_F ((void *)START, VL); START += VL;                    \
    NT vs2 = LD_F ((void *)START, VL); START += VL;                    \
    NT vs3 = LD_F ((void *)START, VL); START += VL;                    \
                                                                       \
    asm volatile("nop" ::: "memory");                                  \
                                                                       \
    WT vd0 = OUT_F (vs0, VL);                                          \
    WT vd1 = OUT_F (vs1, VL);                                          \
    WT vd2 = OUT_F (vs2, VL);                                          \
    WT vd3 = OUT_F (vs3, VL);                                          \
                                                                       \
    asm volatile("nop" ::: "memory");                                  \
                                                                       \
    ST_F ((void *)out, vd0, VL); OUT += VL;                            \
    ST_F ((void *)out, vd1, VL); OUT += VL;                            \
    ST_F ((void *)out, vd2, VL); OUT += VL;                            \
    ST_F ((void *)out, vd3, VL); OUT += VL;                            \

#define LOOP_UNARY_BODY_X8(NT, WT, LD_F, OUT_F, ST_F, OUT, START, VL)  \
    NT vs0 = LD_F ((void *)START, VL); START += VL;                    \
    NT vs1 = LD_F ((void *)START, VL); START += VL;                    \
    NT vs2 = LD_F ((void *)START, VL); START += VL;                    \
    NT vs3 = LD_F ((void *)START, VL); START += VL;                    \
    NT vs4 = LD_F ((void *)START, VL); START += VL;                    \
    NT vs5 = LD_F ((void *)START, VL); START += VL;                    \
    NT vs6 = LD_F ((void *)START, VL); START += VL;                    \
    NT vs7 = LD_F ((void *)START, VL); START += VL;                    \
                                                                       \
    asm volatile("nop" ::: "memory");                                  \
                                                                       \
    WT vd0 = OUT_F (vs0, VL);                                          \
    WT vd1 = OUT_F (vs1, VL);                                          \
    WT vd2 = OUT_F (vs2, VL);                                          \
    WT vd3 = OUT_F (vs3, VL);                                          \
    WT vd4 = OUT_F (vs4, VL);                                          \
    WT vd5 = OUT_F (vs5, VL);                                          \
    WT vd6 = OUT_F (vs6, VL);                                          \
    WT vd7 = OUT_F (vs7, VL);                                          \
                                                                       \
    asm volatile("nop" ::: "memory");                                  \
                                                                       \
    ST_F ((void *)out, vd0, VL); OUT += VL;                            \
    ST_F ((void *)out, vd1, VL); OUT += VL;                            \
    ST_F ((void *)out, vd2, VL); OUT += VL;                            \
    ST_F ((void *)out, vd3, VL); OUT += VL;                            \
    ST_F ((void *)out, vd4, VL); OUT += VL;                            \
    ST_F ((void *)out, vd5, VL); OUT += VL;                            \
    ST_F ((void *)out, vd6, VL); OUT += VL;                            \
    ST_F ((void *)out, vd7, VL); OUT += VL;                            \

#define LOOP_UNARY_BODY_X16(NT, WT, LD_F, OUT_F, ST_F, OUT, START, VL) \
    NT vs0 = LD_F ((void *)START, VL); START += VL;                    \
    NT vs1 = LD_F ((void *)START, VL); START += VL;                    \
    NT vs2 = LD_F ((void *)START, VL); START += VL;                    \
    NT vs3 = LD_F ((void *)START, VL); START += VL;                    \
    NT vs4 = LD_F ((void *)START, VL); START += VL;                    \
    NT vs5 = LD_F ((void *)START, VL); START += VL;                    \
    NT vs6 = LD_F ((void *)START, VL); START += VL;                    \
    NT vs7 = LD_F ((void *)START, VL); START += VL;                    \
    NT vs8 = LD_F ((void *)START, VL); START += VL;                    \
    NT vs9 = LD_F ((void *)START, VL); START += VL;                    \
    NT vs10 = LD_F ((void *)START, VL); START += VL;                   \
    NT vs11 = LD_F ((void *)START, VL); START += VL;                   \
    NT vs12 = LD_F ((void *)START, VL); START += VL;                   \
    NT vs13 = LD_F ((void *)START, VL); START += VL;                   \
    NT vs14 = LD_F ((void *)START, VL); START += VL;                   \
    NT vs15 = LD_F ((void *)START, VL); START += VL;                   \
                                                                       \
    asm volatile("nop" ::: "memory");                                  \
                                                                       \
    WT vd0 = OUT_F (vs0, VL);                                          \
    WT vd1 = OUT_F (vs1, VL);                                          \
    WT vd2 = OUT_F (vs2, VL);                                          \
    WT vd3 = OUT_F (vs3, VL);                                          \
    WT vd4 = OUT_F (vs4, VL);                                          \
    WT vd5 = OUT_F (vs5, VL);                                          \
    WT vd6 = OUT_F (vs6, VL);                                          \
    WT vd7 = OUT_F (vs7, VL);                                          \
    WT vd8 = OUT_F (vs8, VL);                                          \
    WT vd9 = OUT_F (vs9, VL);                                          \
    WT vd10 = OUT_F (vs10, VL);                                        \
    WT vd11 = OUT_F (vs11, VL);                                        \
    WT vd12 = OUT_F (vs12, VL);                                        \
    WT vd13 = OUT_F (vs13, VL);                                        \
    WT vd14 = OUT_F (vs14, VL);                                        \
    WT vd15 = OUT_F (vs15, VL);                                        \
                                                                       \
    asm volatile("nop" ::: "memory");                                  \
                                                                       \
    ST_F ((void *)out, vd0, VL); OUT += VL;                            \
    ST_F ((void *)out, vd1, VL); OUT += VL;                            \
    ST_F ((void *)out, vd2, VL); OUT += VL;                            \
    ST_F ((void *)out, vd3, VL); OUT += VL;                            \
    ST_F ((void *)out, vd4, VL); OUT += VL;                            \
    ST_F ((void *)out, vd5, VL); OUT += VL;                            \
    ST_F ((void *)out, vd6, VL); OUT += VL;                            \
    ST_F ((void *)out, vd7, VL); OUT += VL;                            \
    ST_F ((void *)out, vd8, VL); OUT += VL;                            \
    ST_F ((void *)out, vd9, VL); OUT += VL;                            \
    ST_F ((void *)out, vd10, VL); OUT += VL;                           \
    ST_F ((void *)out, vd11, VL); OUT += VL;                           \
    ST_F ((void *)out, vd12, VL); OUT += VL;                           \
    ST_F ((void *)out, vd13, VL); OUT += VL;                           \
    ST_F ((void *)out, vd14, VL); OUT += VL;                           \
    ST_F ((void *)out, vd15, VL); OUT += VL;                           \

#define LOOP_WIDEN_BINARY_BODY_X8(NT, WT, LD_NF, LD_WF, OUT_F, ST_F, OUT,   \
				   START, VL)                               \
    NT vs0 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs1 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs2 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs3 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs4 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs5 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs6 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs7 = LD_NF ((void *)START, VL); START += VL;                        \
    WT vw0 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw1 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw2 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw3 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw4 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw5 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw6 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw7 = LD_WF ((void *)START, VL); START += VL;                        \
                                                                            \
    asm volatile("nop" ::: "memory");                                       \
                                                                            \
    WT vd0 = OUT_F (vw0, vs0, VL);                                          \
    WT vd1 = OUT_F (vw1, vs1, VL);                                          \
    WT vd2 = OUT_F (vw2, vs2, VL);                                          \
    WT vd3 = OUT_F (vw3, vs3, VL);                                          \
    WT vd4 = OUT_F (vw4, vs4, VL);                                          \
    WT vd5 = OUT_F (vw5, vs5, VL);                                          \
    WT vd6 = OUT_F (vw6, vs6, VL);                                          \
    WT vd7 = OUT_F (vw7, vs7, VL);                                          \
                                                                            \
    asm volatile("nop" ::: "memory");                                       \
                                                                            \
    ST_F ((void *)out, vd0, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd1, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd2, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd3, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd4, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd5, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd6, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd7, VL); OUT += VL;                                 \

#define LOOP_WIDEN_BINARY_BODY_X4(NT, WT, LD_NF, LD_WF, OUT_F, ST_F, OUT,   \
				   START, VL)                               \
    NT vs0 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs1 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs2 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs3 = LD_NF ((void *)START, VL); START += VL;                        \
    WT vw0 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw1 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw2 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw3 = LD_WF ((void *)START, VL); START += VL;                        \
                                                                            \
    asm volatile("nop" ::: "memory");                                       \
                                                                            \
    WT vd0 = OUT_F (vw0, vs0, VL);                                          \
    WT vd1 = OUT_F (vw1, vs1, VL);                                          \
    WT vd2 = OUT_F (vw2, vs2, VL);                                          \
    WT vd3 = OUT_F (vw3, vs3, VL);                                          \
                                                                            \
    asm volatile("nop" ::: "memory");                                       \
                                                                            \
    ST_F ((void *)out, vd0, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd1, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd2, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd3, VL); OUT += VL;                                 \

#define LOOP_WIDEN_BINARY_BODY_X16(NT, WT, LD_NF, LD_WF, OUT_F, ST_F, OUT,  \
				   START, VL)                               \
    NT vs0 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs1 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs2 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs3 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs4 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs5 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs6 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs7 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs8 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs9 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs10 = LD_NF ((void *)START, VL); START += VL;                       \
    NT vs11 = LD_NF ((void *)START, VL); START += VL;                       \
    NT vs12 = LD_NF ((void *)START, VL); START += VL;                       \
    NT vs13 = LD_NF ((void *)START, VL); START += VL;                       \
    NT vs14 = LD_NF ((void *)START, VL); START += VL;                       \
    NT vs15 = LD_NF ((void *)START, VL); START += VL;                       \
    WT vw0 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw1 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw2 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw3 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw4 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw5 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw6 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw7 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw8 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw9 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw10 = LD_WF ((void *)START, VL); START += VL;                       \
    WT vw11 = LD_WF ((void *)START, VL); START += VL;                       \
    WT vw12 = LD_WF ((void *)START, VL); START += VL;                       \
    WT vw13 = LD_WF ((void *)START, VL); START += VL;                       \
    WT vw14 = LD_WF ((void *)START, VL); START += VL;                       \
    WT vw15 = LD_WF ((void *)START, VL); START += VL;                       \
                                                                            \
    asm volatile("nop" ::: "memory");                                       \
                                                                            \
    WT vd0 = OUT_F (vw0, vs0, VL);                                          \
    WT vd1 = OUT_F (vw1, vs1, VL);                                          \
    WT vd2 = OUT_F (vw2, vs2, VL);                                          \
    WT vd3 = OUT_F (vw3, vs3, VL);                                          \
    WT vd4 = OUT_F (vw4, vs4, VL);                                          \
    WT vd5 = OUT_F (vw5, vs5, VL);                                          \
    WT vd6 = OUT_F (vw6, vs6, VL);                                          \
    WT vd7 = OUT_F (vw7, vs7, VL);                                          \
    WT vd8 = OUT_F (vw8, vs8, VL);                                          \
    WT vd9 = OUT_F (vw9, vs9, VL);                                          \
    WT vd10 = OUT_F (vw10, vs10, VL);                                       \
    WT vd11 = OUT_F (vw11, vs11, VL);                                       \
    WT vd12 = OUT_F (vw12, vs12, VL);                                       \
    WT vd13 = OUT_F (vw13, vs13, VL);                                       \
    WT vd14 = OUT_F (vw14, vs14, VL);                                       \
    WT vd15 = OUT_F (vw15, vs15, VL);                                       \
                                                                            \
    asm volatile("nop" ::: "memory");                                       \
                                                                            \
    ST_F ((void *)out, vd0, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd1, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd2, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd3, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd4, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd5, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd6, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd7, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd8, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd9, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd10, VL); OUT += VL;                                \
    ST_F ((void *)out, vd11, VL); OUT += VL;                                \
    ST_F ((void *)out, vd12, VL); OUT += VL;                                \
    ST_F ((void *)out, vd13, VL); OUT += VL;                                \
    ST_F ((void *)out, vd14, VL); OUT += VL;                                \
    ST_F ((void *)out, vd15, VL); OUT += VL;                                \

#define LOOP_DUAL_WIDEN_BINARY_BODY_X4(NT, WT, LD_NF, OUT_F, ST_F, OUT,     \
					START, VL)                          \
    NT vs0 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs1 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs2 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs3 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vt0 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vt1 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vt2 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vt3 = LD_NF ((void *)START, VL); START += VL;                        \
                                                                            \
    asm volatile("nop" ::: "memory");                                       \
                                                                            \
    WT vd0 = OUT_F (vs0, vt0, VL);                                          \
    WT vd1 = OUT_F (vs1, vt1, VL);                                          \
    WT vd2 = OUT_F (vs2, vt2, VL);                                          \
    WT vd3 = OUT_F (vs3, vt3, VL);                                          \
                                                                            \
    asm volatile("nop" ::: "memory");                                       \
                                                                            \
    ST_F ((void *)out, vd0, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd1, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd2, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd3, VL); OUT += VL;                                 \

#define LOOP_DUAL_WIDEN_BINARY_BODY_X8(NT, WT, LD_NF, OUT_F, ST_F, OUT,     \
					START, VL)                          \
    NT vs0 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs1 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs2 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs3 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs4 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs5 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs6 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs7 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vt0 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vt1 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vt2 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vt3 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vt4 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vt5 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vt6 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vt7 = LD_NF ((void *)START, VL); START += VL;                        \
                                                                            \
    asm volatile("nop" ::: "memory");                                       \
                                                                            \
    WT vd0 = OUT_F (vs0, vt0, VL);                                          \
    WT vd1 = OUT_F (vs1, vt1, VL);                                          \
    WT vd2 = OUT_F (vs2, vt2, VL);                                          \
    WT vd3 = OUT_F (vs3, vt3, VL);                                          \
    WT vd4 = OUT_F (vs4, vt4, VL);                                          \
    WT vd5 = OUT_F (vs5, vt5, VL);                                          \
    WT vd6 = OUT_F (vs6, vt6, VL);                                          \
    WT vd7 = OUT_F (vs7, vt7, VL);                                          \
                                                                            \
    asm volatile("nop" ::: "memory");                                       \
                                                                            \
    ST_F ((void *)out, vd0, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd1, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd2, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd3, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd4, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd5, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd6, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd7, VL); OUT += VL;                                 \

#define LOOP_DUAL_WIDEN_BINARY_BODY_X16(NT, WT, LD_NF, OUT_F, ST_F, OUT,    \
					START, VL)                          \
    NT vs0 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs1 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs2 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs3 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs4 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs5 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs6 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs7 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs8 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs9 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs10 = LD_NF ((void *)START, VL); START += VL;                       \
    NT vs11 = LD_NF ((void *)START, VL); START += VL;                       \
    NT vs12 = LD_NF ((void *)START, VL); START += VL;                       \
    NT vs13 = LD_NF ((void *)START, VL); START += VL;                       \
    NT vs14 = LD_NF ((void *)START, VL); START += VL;                       \
    NT vs15 = LD_NF ((void *)START, VL); START += VL;                       \
    NT vt0 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vt1 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vt2 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vt3 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vt4 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vt5 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vt6 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vt7 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vt8 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vt9 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vt10 = LD_NF ((void *)START, VL); START += VL;                       \
    NT vt11 = LD_NF ((void *)START, VL); START += VL;                       \
    NT vt12 = LD_NF ((void *)START, VL); START += VL;                       \
    NT vt13 = LD_NF ((void *)START, VL); START += VL;                       \
    NT vt14 = LD_NF ((void *)START, VL); START += VL;                       \
    NT vt15 = LD_NF ((void *)START, VL); START += VL;                       \
                                                                            \
    asm volatile("nop" ::: "memory");                                       \
                                                                            \
    WT vd0 = OUT_F (vs0, vt0, VL);                                          \
    WT vd1 = OUT_F (vs1, vt1, VL);                                          \
    WT vd2 = OUT_F (vs2, vt2, VL);                                          \
    WT vd3 = OUT_F (vs3, vt3, VL);                                          \
    WT vd4 = OUT_F (vs4, vt4, VL);                                          \
    WT vd5 = OUT_F (vs5, vt5, VL);                                          \
    WT vd6 = OUT_F (vs6, vt6, VL);                                          \
    WT vd7 = OUT_F (vs7, vt7, VL);                                          \
    WT vd8 = OUT_F (vs8, vt8, VL);                                          \
    WT vd9 = OUT_F (vs9, vt9, VL);                                          \
    WT vd10 = OUT_F (vs10, vt10, VL);                                       \
    WT vd11 = OUT_F (vs11, vt11, VL);                                       \
    WT vd12 = OUT_F (vs12, vt12, VL);                                       \
    WT vd13 = OUT_F (vs13, vt13, VL);                                       \
    WT vd14 = OUT_F (vs14, vt14, VL);                                       \
    WT vd15 = OUT_F (vs15, vt15, VL);                                       \
                                                                            \
    asm volatile("nop" ::: "memory");                                       \
                                                                            \
    ST_F ((void *)out, vd0, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd1, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd2, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd3, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd4, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd5, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd6, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd7, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd8, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd9, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd10, VL); OUT += VL;                                \
    ST_F ((void *)out, vd11, VL); OUT += VL;                                \
    ST_F ((void *)out, vd12, VL); OUT += VL;                                \
    ST_F ((void *)out, vd13, VL); OUT += VL;                                \
    ST_F ((void *)out, vd14, VL); OUT += VL;                                \
    ST_F ((void *)out, vd15, VL); OUT += VL;                                \

#define LOOP_DUAL_WIDEN_BINARY_VX_BODY_X4(NT, WT, LD_NF, OUT_F, ST_F, OUT,  \
					   START, X, VL)                    \
    NT vs0 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs1 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs2 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs3 = LD_NF ((void *)START, VL); START += VL;                        \
                                                                            \
    asm volatile("nop" ::: "memory");                                       \
                                                                            \
    WT vd0 = OUT_F (vs0, X, VL);                                            \
    WT vd1 = OUT_F (vs1, X, VL);                                            \
    WT vd2 = OUT_F (vs2, X, VL);                                            \
    WT vd3 = OUT_F (vs3, X, VL);                                            \
                                                                            \
    asm volatile("nop" ::: "memory");                                       \
                                                                            \
    ST_F ((void *)out, vd0, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd1, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd2, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd3, VL); OUT += VL;                                 \

#define LOOP_DUAL_WIDEN_BINARY_VX_BODY_X8(NT, WT, LD_NF, OUT_F, ST_F, OUT,  \
					   START, X, VL)                    \
    NT vs0 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs1 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs2 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs3 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs4 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs5 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs6 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs7 = LD_NF ((void *)START, VL); START += VL;                        \
                                                                            \
    asm volatile("nop" ::: "memory");                                       \
                                                                            \
    WT vd0 = OUT_F (vs0, X, VL);                                            \
    WT vd1 = OUT_F (vs1, X, VL);                                            \
    WT vd2 = OUT_F (vs2, X, VL);                                            \
    WT vd3 = OUT_F (vs3, X, VL);                                            \
    WT vd4 = OUT_F (vs4, X, VL);                                            \
    WT vd5 = OUT_F (vs5, X, VL);                                            \
    WT vd6 = OUT_F (vs6, X, VL);                                            \
    WT vd7 = OUT_F (vs7, X, VL);                                            \
                                                                            \
    asm volatile("nop" ::: "memory");                                       \
                                                                            \
    ST_F ((void *)out, vd0, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd1, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd2, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd3, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd4, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd5, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd6, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd7, VL); OUT += VL;                                 \

#define LOOP_DUAL_WIDEN_BINARY_VX_BODY_X16(NT, WT, LD_NF, OUT_F, ST_F, OUT, \
					   START, X, VL)                    \
    NT vs0 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs1 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs2 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs3 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs4 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs5 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs6 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs7 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs8 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs9 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs10 = LD_NF ((void *)START, VL); START += VL;                       \
    NT vs11 = LD_NF ((void *)START, VL); START += VL;                       \
    NT vs12 = LD_NF ((void *)START, VL); START += VL;                       \
    NT vs13 = LD_NF ((void *)START, VL); START += VL;                       \
    NT vs14 = LD_NF ((void *)START, VL); START += VL;                       \
    NT vs15 = LD_NF ((void *)START, VL); START += VL;                       \
                                                                            \
    asm volatile("nop" ::: "memory");                                       \
                                                                            \
    WT vd0 = OUT_F (vs0, X, VL);                                            \
    WT vd1 = OUT_F (vs1, X, VL);                                            \
    WT vd2 = OUT_F (vs2, X, VL);                                            \
    WT vd3 = OUT_F (vs3, X, VL);                                            \
    WT vd4 = OUT_F (vs4, X, VL);                                            \
    WT vd5 = OUT_F (vs5, X, VL);                                            \
    WT vd6 = OUT_F (vs6, X, VL);                                            \
    WT vd7 = OUT_F (vs7, X, VL);                                            \
    WT vd8 = OUT_F (vs8, X, VL);                                            \
    WT vd9 = OUT_F (vs9, X, VL);                                            \
    WT vd10 = OUT_F (vs10, X, VL);                                          \
    WT vd11 = OUT_F (vs11, X, VL);                                          \
    WT vd12 = OUT_F (vs12, X, VL);                                          \
    WT vd13 = OUT_F (vs13, X, VL);                                          \
    WT vd14 = OUT_F (vs14, X, VL);                                          \
    WT vd15 = OUT_F (vs15, X, VL);                                          \
                                                                            \
    asm volatile("nop" ::: "memory");                                       \
                                                                            \
    ST_F ((void *)out, vd0, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd1, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd2, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd3, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd4, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd5, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd6, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd7, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd8, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd9, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd10, VL); OUT += VL;                                \
    ST_F ((void *)out, vd11, VL); OUT += VL;                                \
    ST_F ((void *)out, vd12, VL); OUT += VL;                                \
    ST_F ((void *)out, vd13, VL); OUT += VL;                                \
    ST_F ((void *)out, vd14, VL); OUT += VL;                                \
    ST_F ((void *)out, vd15, VL); OUT += VL;                                \

#define LOOP_DUAL_WIDEN_BINARY_BODY_SU_X4(NT, NUT, WT, LD_NF,               \
					   LD_NUF, OUT_F, ST_F, OUT,        \
					   START, VL)                       \
    NT vs0 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs1 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs2 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs3 = LD_NF ((void *)START, VL); START += VL;                        \
    NUT vt0 = LD_NUF ((void *)START, VL); START += VL;                      \
    NUT vt1 = LD_NUF ((void *)START, VL); START += VL;                      \
    NUT vt2 = LD_NUF ((void *)START, VL); START += VL;                      \
    NUT vt3 = LD_NUF ((void *)START, VL); START += VL;                      \
                                                                            \
    asm volatile("nop" ::: "memory");                                       \
                                                                            \
    WT vd0 = OUT_F (vs0, vt0, VL);                                          \
    WT vd1 = OUT_F (vs1, vt1, VL);                                          \
    WT vd2 = OUT_F (vs2, vt2, VL);                                          \
    WT vd3 = OUT_F (vs3, vt3, VL);                                          \
                                                                            \
    asm volatile("nop" ::: "memory");                                       \
                                                                            \
    ST_F ((void *)out, vd0, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd1, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd2, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd3, VL); OUT += VL;                                 \

#define LOOP_DUAL_WIDEN_BINARY_BODY_SU_X8(NT, NUT, WT, LD_NF,               \
					   LD_NUF, OUT_F, ST_F, OUT,        \
					   START, VL)                       \
    NT vs0 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs1 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs2 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs3 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs4 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs5 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs6 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs7 = LD_NF ((void *)START, VL); START += VL;                        \
    NUT vt0 = LD_NUF ((void *)START, VL); START += VL;                      \
    NUT vt1 = LD_NUF ((void *)START, VL); START += VL;                      \
    NUT vt2 = LD_NUF ((void *)START, VL); START += VL;                      \
    NUT vt3 = LD_NUF ((void *)START, VL); START += VL;                      \
    NUT vt4 = LD_NUF ((void *)START, VL); START += VL;                      \
    NUT vt5 = LD_NUF ((void *)START, VL); START += VL;                      \
    NUT vt6 = LD_NUF ((void *)START, VL); START += VL;                      \
    NUT vt7 = LD_NUF ((void *)START, VL); START += VL;                      \
                                                                            \
    asm volatile("nop" ::: "memory");                                       \
                                                                            \
    WT vd0 = OUT_F (vs0, vt0, VL);                                          \
    WT vd1 = OUT_F (vs1, vt1, VL);                                          \
    WT vd2 = OUT_F (vs2, vt2, VL);                                          \
    WT vd3 = OUT_F (vs3, vt3, VL);                                          \
    WT vd4 = OUT_F (vs4, vt4, VL);                                          \
    WT vd5 = OUT_F (vs5, vt5, VL);                                          \
    WT vd6 = OUT_F (vs6, vt6, VL);                                          \
    WT vd7 = OUT_F (vs7, vt7, VL);                                          \
                                                                            \
    asm volatile("nop" ::: "memory");                                       \
                                                                            \
    ST_F ((void *)out, vd0, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd1, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd2, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd3, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd4, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd5, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd6, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd7, VL); OUT += VL;                                 \

#define LOOP_DUAL_WIDEN_BINARY_BODY_SU_X16(NT, NUT, WT, LD_NF,              \
					   LD_NUF, OUT_F, ST_F, OUT,        \
					   START, VL)                       \
    NT vs0 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs1 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs2 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs3 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs4 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs5 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs6 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs7 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs8 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs9 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs10 = LD_NF ((void *)START, VL); START += VL;                       \
    NT vs11 = LD_NF ((void *)START, VL); START += VL;                       \
    NT vs12 = LD_NF ((void *)START, VL); START += VL;                       \
    NT vs13 = LD_NF ((void *)START, VL); START += VL;                       \
    NT vs14 = LD_NF ((void *)START, VL); START += VL;                       \
    NT vs15 = LD_NF ((void *)START, VL); START += VL;                       \
    NUT vt0 = LD_NUF ((void *)START, VL); START += VL;                      \
    NUT vt1 = LD_NUF ((void *)START, VL); START += VL;                      \
    NUT vt2 = LD_NUF ((void *)START, VL); START += VL;                      \
    NUT vt3 = LD_NUF ((void *)START, VL); START += VL;                      \
    NUT vt4 = LD_NUF ((void *)START, VL); START += VL;                      \
    NUT vt5 = LD_NUF ((void *)START, VL); START += VL;                      \
    NUT vt6 = LD_NUF ((void *)START, VL); START += VL;                      \
    NUT vt7 = LD_NUF ((void *)START, VL); START += VL;                      \
    NUT vt8 = LD_NUF ((void *)START, VL); START += VL;                      \
    NUT vt9 = LD_NUF ((void *)START, VL); START += VL;                      \
    NUT vt10 = LD_NUF ((void *)START, VL); START += VL;                     \
    NUT vt11 = LD_NUF ((void *)START, VL); START += VL;                     \
    NUT vt12 = LD_NUF ((void *)START, VL); START += VL;                     \
    NUT vt13 = LD_NUF ((void *)START, VL); START += VL;                     \
    NUT vt14 = LD_NUF ((void *)START, VL); START += VL;                     \
    NUT vt15 = LD_NUF ((void *)START, VL); START += VL;                     \
                                                                            \
    asm volatile("nop" ::: "memory");                                       \
                                                                            \
    WT vd0 = OUT_F (vs0, vt0, VL);                                          \
    WT vd1 = OUT_F (vs1, vt1, VL);                                          \
    WT vd2 = OUT_F (vs2, vt2, VL);                                          \
    WT vd3 = OUT_F (vs3, vt3, VL);                                          \
    WT vd4 = OUT_F (vs4, vt4, VL);                                          \
    WT vd5 = OUT_F (vs5, vt5, VL);                                          \
    WT vd6 = OUT_F (vs6, vt6, VL);                                          \
    WT vd7 = OUT_F (vs7, vt7, VL);                                          \
    WT vd8 = OUT_F (vs8, vt8, VL);                                          \
    WT vd9 = OUT_F (vs9, vt9, VL);                                          \
    WT vd10 = OUT_F (vs10, vt10, VL);                                       \
    WT vd11 = OUT_F (vs11, vt11, VL);                                       \
    WT vd12 = OUT_F (vs12, vt12, VL);                                       \
    WT vd13 = OUT_F (vs13, vt13, VL);                                       \
    WT vd14 = OUT_F (vs14, vt14, VL);                                       \
    WT vd15 = OUT_F (vs15, vt15, VL);                                       \
                                                                            \
    asm volatile("nop" ::: "memory");                                       \
                                                                            \
    ST_F ((void *)out, vd0, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd1, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd2, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd3, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd4, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd5, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd6, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd7, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd8, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd9, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd10, VL); OUT += VL;                                \
    ST_F ((void *)out, vd11, VL); OUT += VL;                                \
    ST_F ((void *)out, vd12, VL); OUT += VL;                                \
    ST_F ((void *)out, vd13, VL); OUT += VL;                                \
    ST_F ((void *)out, vd14, VL); OUT += VL;                                \
    ST_F ((void *)out, vd15, VL); OUT += VL;                                \

#define LOOP_DUAL_WIDEN_TERNARY_BODY_X4(NT, WT, LD_NF, LD_WF, OUT_F, ST_F,  \
					 OUT, START, VL)                    \
    NT vs0 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs1 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs2 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs3 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vt0 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vt1 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vt2 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vt3 = LD_NF ((void *)START, VL); START += VL;                        \
    WT vw0 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw1 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw2 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw3 = LD_WF ((void *)START, VL); START += VL;                        \
                                                                            \
    asm volatile("nop" ::: "memory");                                       \
                                                                            \
    WT vd0 = OUT_F (vw0, vs0, vt0, VL);                                     \
    WT vd1 = OUT_F (vw1, vs1, vt1, VL);                                     \
    WT vd2 = OUT_F (vw2, vs2, vt2, VL);                                     \
    WT vd3 = OUT_F (vw3, vs3, vt3, VL);                                     \
                                                                            \
    asm volatile("nop" ::: "memory");                                       \
                                                                            \
    ST_F ((void *)out, vd0, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd1, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd2, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd3, VL); OUT += VL;                                 \

#define LOOP_DUAL_WIDEN_TERNARY_BODY_X8(NT, WT, LD_NF, LD_WF, OUT_F, ST_F,  \
					 OUT, START, VL)                    \
    NT vs0 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs1 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs2 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs3 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs4 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs5 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs6 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs7 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vt0 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vt1 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vt2 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vt3 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vt4 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vt5 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vt6 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vt7 = LD_NF ((void *)START, VL); START += VL;                        \
    WT vw0 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw1 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw2 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw3 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw4 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw5 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw6 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw7 = LD_WF ((void *)START, VL); START += VL;                        \
                                                                            \
    asm volatile("nop" ::: "memory");                                       \
                                                                            \
    WT vd0 = OUT_F (vw0, vs0, vt0, VL);                                     \
    WT vd1 = OUT_F (vw1, vs1, vt1, VL);                                     \
    WT vd2 = OUT_F (vw2, vs2, vt2, VL);                                     \
    WT vd3 = OUT_F (vw3, vs3, vt3, VL);                                     \
    WT vd4 = OUT_F (vw4, vs4, vt4, VL);                                     \
    WT vd5 = OUT_F (vw5, vs5, vt5, VL);                                     \
    WT vd6 = OUT_F (vw6, vs6, vt6, VL);                                     \
    WT vd7 = OUT_F (vw7, vs7, vt7, VL);                                     \
                                                                            \
    asm volatile("nop" ::: "memory");                                       \
                                                                            \
    ST_F ((void *)out, vd0, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd1, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd2, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd3, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd4, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd5, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd6, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd7, VL); OUT += VL;                                 \

#define LOOP_DUAL_WIDEN_TERNARY_BODY_X16(NT, WT, LD_NF, LD_WF, OUT_F, ST_F, \
					 OUT, START, VL)                    \
    NT vs0 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs1 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs2 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs3 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs4 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs5 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs6 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs7 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs8 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs9 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs10 = LD_NF ((void *)START, VL); START += VL;                       \
    NT vs11 = LD_NF ((void *)START, VL); START += VL;                       \
    NT vs12 = LD_NF ((void *)START, VL); START += VL;                       \
    NT vs13 = LD_NF ((void *)START, VL); START += VL;                       \
    NT vs14 = LD_NF ((void *)START, VL); START += VL;                       \
    NT vs15 = LD_NF ((void *)START, VL); START += VL;                       \
    NT vt0 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vt1 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vt2 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vt3 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vt4 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vt5 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vt6 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vt7 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vt8 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vt9 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vt10 = LD_NF ((void *)START, VL); START += VL;                       \
    NT vt11 = LD_NF ((void *)START, VL); START += VL;                       \
    NT vt12 = LD_NF ((void *)START, VL); START += VL;                       \
    NT vt13 = LD_NF ((void *)START, VL); START += VL;                       \
    NT vt14 = LD_NF ((void *)START, VL); START += VL;                       \
    NT vt15 = LD_NF ((void *)START, VL); START += VL;                       \
    WT vw0 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw1 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw2 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw3 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw4 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw5 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw6 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw7 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw8 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw9 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw10 = LD_WF ((void *)START, VL); START += VL;                       \
    WT vw11 = LD_WF ((void *)START, VL); START += VL;                       \
    WT vw12 = LD_WF ((void *)START, VL); START += VL;                       \
    WT vw13 = LD_WF ((void *)START, VL); START += VL;                       \
    WT vw14 = LD_WF ((void *)START, VL); START += VL;                       \
    WT vw15 = LD_WF ((void *)START, VL); START += VL;                       \
                                                                            \
    asm volatile("nop" ::: "memory");                                       \
                                                                            \
    WT vd0 = OUT_F (vw0, vs0, vt0, VL);                                     \
    WT vd1 = OUT_F (vw1, vs1, vt1, VL);                                     \
    WT vd2 = OUT_F (vw2, vs2, vt2, VL);                                     \
    WT vd3 = OUT_F (vw3, vs3, vt3, VL);                                     \
    WT vd4 = OUT_F (vw4, vs4, vt4, VL);                                     \
    WT vd5 = OUT_F (vw5, vs5, vt5, VL);                                     \
    WT vd6 = OUT_F (vw6, vs6, vt6, VL);                                     \
    WT vd7 = OUT_F (vw7, vs7, vt7, VL);                                     \
    WT vd8 = OUT_F (vw8, vs8, vt8, VL);                                     \
    WT vd9 = OUT_F (vw9, vs9, vt9, VL);                                     \
    WT vd10 = OUT_F (vw10, vs10, vt10, VL);                                 \
    WT vd11 = OUT_F (vw11, vs11, vt11, VL);                                 \
    WT vd12 = OUT_F (vw12, vs12, vt12, VL);                                 \
    WT vd13 = OUT_F (vw13, vs13, vt13, VL);                                 \
    WT vd14 = OUT_F (vw14, vs14, vt14, VL);                                 \
    WT vd15 = OUT_F (vw15, vs15, vt15, VL);                                 \
                                                                            \
    asm volatile("nop" ::: "memory");                                       \
                                                                            \
    ST_F ((void *)out, vd0, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd1, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd2, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd3, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd4, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd5, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd6, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd7, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd8, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd9, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd10, VL); OUT += VL;                                \
    ST_F ((void *)out, vd11, VL); OUT += VL;                                \
    ST_F ((void *)out, vd12, VL); OUT += VL;                                \
    ST_F ((void *)out, vd13, VL); OUT += VL;                                \
    ST_F ((void *)out, vd14, VL); OUT += VL;                                \
    ST_F ((void *)out, vd15, VL); OUT += VL;                                \

#define LOOP_DUAL_WIDEN_TERNARY_VX_BODY_X4(NT, WT, LD_NF, LD_WF, OUT_F,     \
					    ST_F, OUT, START, X, VL)        \
    NT vs0 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs1 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs2 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs3 = LD_NF ((void *)START, VL); START += VL;                        \
    WT vw0 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw1 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw2 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw3 = LD_WF ((void *)START, VL); START += VL;                        \
                                                                            \
    asm volatile("nop" ::: "memory");                                       \
                                                                            \
    WT vd0 = OUT_F (vw0, X, vs0, VL);                                       \
    WT vd1 = OUT_F (vw1, X, vs1, VL);                                       \
    WT vd2 = OUT_F (vw2, X, vs2, VL);                                       \
    WT vd3 = OUT_F (vw3, X, vs3, VL);                                       \
                                                                            \
    asm volatile("nop" ::: "memory");                                       \
                                                                            \
    ST_F ((void *)out, vd0, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd1, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd2, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd3, VL); OUT += VL;                                 \

#define LOOP_DUAL_WIDEN_TERNARY_VX_BODY_X8(NT, WT, LD_NF, LD_WF, OUT_F,     \
					    ST_F, OUT, START, X, VL)        \
    NT vs0 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs1 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs2 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs3 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs4 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs5 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs6 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs7 = LD_NF ((void *)START, VL); START += VL;                        \
    WT vw0 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw1 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw2 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw3 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw4 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw5 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw6 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw7 = LD_WF ((void *)START, VL); START += VL;                        \
                                                                            \
    asm volatile("nop" ::: "memory");                                       \
                                                                            \
    WT vd0 = OUT_F (vw0, X, vs0, VL);                                       \
    WT vd1 = OUT_F (vw1, X, vs1, VL);                                       \
    WT vd2 = OUT_F (vw2, X, vs2, VL);                                       \
    WT vd3 = OUT_F (vw3, X, vs3, VL);                                       \
    WT vd4 = OUT_F (vw4, X, vs4, VL);                                       \
    WT vd5 = OUT_F (vw5, X, vs5, VL);                                       \
    WT vd6 = OUT_F (vw6, X, vs6, VL);                                       \
    WT vd7 = OUT_F (vw7, X, vs7, VL);                                       \
                                                                            \
    asm volatile("nop" ::: "memory");                                       \
                                                                            \
    ST_F ((void *)out, vd0, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd1, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd2, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd3, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd4, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd5, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd6, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd7, VL); OUT += VL;                                 \

#define LOOP_DUAL_WIDEN_TERNARY_VX_BODY_X16(NT, WT, LD_NF, LD_WF, OUT_F,    \
					    ST_F, OUT, START, X, VL)        \
    NT vs0 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs1 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs2 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs3 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs4 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs5 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs6 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs7 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs8 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs9 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs10 = LD_NF ((void *)START, VL); START += VL;                       \
    NT vs11 = LD_NF ((void *)START, VL); START += VL;                       \
    NT vs12 = LD_NF ((void *)START, VL); START += VL;                       \
    NT vs13 = LD_NF ((void *)START, VL); START += VL;                       \
    NT vs14 = LD_NF ((void *)START, VL); START += VL;                       \
    NT vs15 = LD_NF ((void *)START, VL); START += VL;                       \
    WT vw0 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw1 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw2 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw3 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw4 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw5 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw6 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw7 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw8 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw9 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw10 = LD_WF ((void *)START, VL); START += VL;                       \
    WT vw11 = LD_WF ((void *)START, VL); START += VL;                       \
    WT vw12 = LD_WF ((void *)START, VL); START += VL;                       \
    WT vw13 = LD_WF ((void *)START, VL); START += VL;                       \
    WT vw14 = LD_WF ((void *)START, VL); START += VL;                       \
    WT vw15 = LD_WF ((void *)START, VL); START += VL;                       \
                                                                            \
    asm volatile("nop" ::: "memory");                                       \
                                                                            \
    WT vd0 = OUT_F (vw0, X, vs0, VL);                                       \
    WT vd1 = OUT_F (vw1, X, vs1, VL);                                       \
    WT vd2 = OUT_F (vw2, X, vs2, VL);                                       \
    WT vd3 = OUT_F (vw3, X, vs3, VL);                                       \
    WT vd4 = OUT_F (vw4, X, vs4, VL);                                       \
    WT vd5 = OUT_F (vw5, X, vs5, VL);                                       \
    WT vd6 = OUT_F (vw6, X, vs6, VL);                                       \
    WT vd7 = OUT_F (vw7, X, vs7, VL);                                       \
    WT vd8 = OUT_F (vw8, X, vs8, VL);                                       \
    WT vd9 = OUT_F (vw9, X, vs9, VL);                                       \
    WT vd10 = OUT_F (vw10, X, vs10, VL);                                    \
    WT vd11 = OUT_F (vw11, X, vs11, VL);                                    \
    WT vd12 = OUT_F (vw12, X, vs12, VL);                                    \
    WT vd13 = OUT_F (vw13, X, vs13, VL);                                    \
    WT vd14 = OUT_F (vw14, X, vs14, VL);                                    \
    WT vd15 = OUT_F (vw15, X, vs15, VL);                                    \
                                                                            \
    asm volatile("nop" ::: "memory");                                       \
                                                                            \
    ST_F ((void *)out, vd0, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd1, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd2, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd3, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd4, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd5, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd6, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd7, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd8, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd9, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd10, VL); OUT += VL;                                \
    ST_F ((void *)out, vd11, VL); OUT += VL;                                \
    ST_F ((void *)out, vd12, VL); OUT += VL;                                \
    ST_F ((void *)out, vd13, VL); OUT += VL;                                \
    ST_F ((void *)out, vd14, VL); OUT += VL;                                \
    ST_F ((void *)out, vd15, VL); OUT += VL;                                \

#define LOOP_DUAL_WIDEN_TERNARY_BODY_SU_X4(NT, NUT, WT, LD_NF,              \
					    LD_NUF, LD_WF, OUT_F, ST_F,     \
					    OUT, START, VL)                 \
    NT vs0 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs1 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs2 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs3 = LD_NF ((void *)START, VL); START += VL;                        \
    NUT vt0 = LD_NUF ((void *)START, VL); START += VL;                      \
    NUT vt1 = LD_NUF ((void *)START, VL); START += VL;                      \
    NUT vt2 = LD_NUF ((void *)START, VL); START += VL;                      \
    NUT vt3 = LD_NUF ((void *)START, VL); START += VL;                      \
    WT vw0 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw1 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw2 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw3 = LD_WF ((void *)START, VL); START += VL;                        \
                                                                            \
    asm volatile("nop" ::: "memory");                                       \
                                                                            \
    WT vd0 = OUT_F (vw0, vs0, vt0, VL);                                     \
    WT vd1 = OUT_F (vw1, vs1, vt1, VL);                                     \
    WT vd2 = OUT_F (vw2, vs2, vt2, VL);                                     \
    WT vd3 = OUT_F (vw3, vs3, vt3, VL);                                     \
                                                                            \
    asm volatile("nop" ::: "memory");                                       \
                                                                            \
    ST_F ((void *)out, vd0, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd1, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd2, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd3, VL); OUT += VL;                                 \

#define LOOP_DUAL_WIDEN_TERNARY_BODY_SU_X8(NT, NUT, WT, LD_NF,              \
					    LD_NUF, LD_WF, OUT_F, ST_F,     \
					    OUT, START, VL)                 \
    NT vs0 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs1 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs2 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs3 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs4 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs5 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs6 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs7 = LD_NF ((void *)START, VL); START += VL;                        \
    NUT vt0 = LD_NUF ((void *)START, VL); START += VL;                      \
    NUT vt1 = LD_NUF ((void *)START, VL); START += VL;                      \
    NUT vt2 = LD_NUF ((void *)START, VL); START += VL;                      \
    NUT vt3 = LD_NUF ((void *)START, VL); START += VL;                      \
    NUT vt4 = LD_NUF ((void *)START, VL); START += VL;                      \
    NUT vt5 = LD_NUF ((void *)START, VL); START += VL;                      \
    NUT vt6 = LD_NUF ((void *)START, VL); START += VL;                      \
    NUT vt7 = LD_NUF ((void *)START, VL); START += VL;                      \
    WT vw0 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw1 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw2 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw3 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw4 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw5 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw6 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw7 = LD_WF ((void *)START, VL); START += VL;                        \
                                                                            \
    asm volatile("nop" ::: "memory");                                       \
                                                                            \
    WT vd0 = OUT_F (vw0, vs0, vt0, VL);                                     \
    WT vd1 = OUT_F (vw1, vs1, vt1, VL);                                     \
    WT vd2 = OUT_F (vw2, vs2, vt2, VL);                                     \
    WT vd3 = OUT_F (vw3, vs3, vt3, VL);                                     \
    WT vd4 = OUT_F (vw4, vs4, vt4, VL);                                     \
    WT vd5 = OUT_F (vw5, vs5, vt5, VL);                                     \
    WT vd6 = OUT_F (vw6, vs6, vt6, VL);                                     \
    WT vd7 = OUT_F (vw7, vs7, vt7, VL);                                     \
                                                                            \
    asm volatile("nop" ::: "memory");                                       \
                                                                            \
    ST_F ((void *)out, vd0, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd1, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd2, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd3, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd4, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd5, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd6, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd7, VL); OUT += VL;                                 \

#define LOOP_DUAL_WIDEN_TERNARY_BODY_SU_X16(NT, NUT, WT, LD_NF,             \
					    LD_NUF, LD_WF, OUT_F, ST_F,     \
					    OUT, START, VL)                 \
    NT vs0 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs1 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs2 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs3 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs4 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs5 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs6 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs7 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs8 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs9 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vs10 = LD_NF ((void *)START, VL); START += VL;                       \
    NT vs11 = LD_NF ((void *)START, VL); START += VL;                       \
    NT vs12 = LD_NF ((void *)START, VL); START += VL;                       \
    NT vs13 = LD_NF ((void *)START, VL); START += VL;                       \
    NT vs14 = LD_NF ((void *)START, VL); START += VL;                       \
    NT vs15 = LD_NF ((void *)START, VL); START += VL;                       \
    NUT vt0 = LD_NUF ((void *)START, VL); START += VL;                      \
    NUT vt1 = LD_NUF ((void *)START, VL); START += VL;                      \
    NUT vt2 = LD_NUF ((void *)START, VL); START += VL;                      \
    NUT vt3 = LD_NUF ((void *)START, VL); START += VL;                      \
    NUT vt4 = LD_NUF ((void *)START, VL); START += VL;                      \
    NUT vt5 = LD_NUF ((void *)START, VL); START += VL;                      \
    NUT vt6 = LD_NUF ((void *)START, VL); START += VL;                      \
    NUT vt7 = LD_NUF ((void *)START, VL); START += VL;                      \
    NUT vt8 = LD_NUF ((void *)START, VL); START += VL;                      \
    NUT vt9 = LD_NUF ((void *)START, VL); START += VL;                      \
    NUT vt10 = LD_NUF ((void *)START, VL); START += VL;                     \
    NUT vt11 = LD_NUF ((void *)START, VL); START += VL;                     \
    NUT vt12 = LD_NUF ((void *)START, VL); START += VL;                     \
    NUT vt13 = LD_NUF ((void *)START, VL); START += VL;                     \
    NUT vt14 = LD_NUF ((void *)START, VL); START += VL;                     \
    NUT vt15 = LD_NUF ((void *)START, VL); START += VL;                     \
    WT vw0 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw1 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw2 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw3 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw4 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw5 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw6 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw7 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw8 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw9 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw10 = LD_WF ((void *)START, VL); START += VL;                       \
    WT vw11 = LD_WF ((void *)START, VL); START += VL;                       \
    WT vw12 = LD_WF ((void *)START, VL); START += VL;                       \
    WT vw13 = LD_WF ((void *)START, VL); START += VL;                       \
    WT vw14 = LD_WF ((void *)START, VL); START += VL;                       \
    WT vw15 = LD_WF ((void *)START, VL); START += VL;                       \
                                                                            \
    asm volatile("nop" ::: "memory");                                       \
                                                                            \
    WT vd0 = OUT_F (vw0, vs0, vt0, VL);                                     \
    WT vd1 = OUT_F (vw1, vs1, vt1, VL);                                     \
    WT vd2 = OUT_F (vw2, vs2, vt2, VL);                                     \
    WT vd3 = OUT_F (vw3, vs3, vt3, VL);                                     \
    WT vd4 = OUT_F (vw4, vs4, vt4, VL);                                     \
    WT vd5 = OUT_F (vw5, vs5, vt5, VL);                                     \
    WT vd6 = OUT_F (vw6, vs6, vt6, VL);                                     \
    WT vd7 = OUT_F (vw7, vs7, vt7, VL);                                     \
    WT vd8 = OUT_F (vw8, vs8, vt8, VL);                                     \
    WT vd9 = OUT_F (vw9, vs9, vt9, VL);                                     \
    WT vd10 = OUT_F (vw10, vs10, vt10, VL);                                 \
    WT vd11 = OUT_F (vw11, vs11, vt11, VL);                                 \
    WT vd12 = OUT_F (vw12, vs12, vt12, VL);                                 \
    WT vd13 = OUT_F (vw13, vs13, vt13, VL);                                 \
    WT vd14 = OUT_F (vw14, vs14, vt14, VL);                                 \
    WT vd15 = OUT_F (vw15, vs15, vt15, VL);                                 \
                                                                            \
    asm volatile("nop" ::: "memory");                                       \
                                                                            \
    ST_F ((void *)out, vd0, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd1, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd2, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd3, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd4, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd5, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd6, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd7, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd8, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd9, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd10, VL); OUT += VL;                                \
    ST_F ((void *)out, vd11, VL); OUT += VL;                                \
    ST_F ((void *)out, vd12, VL); OUT += VL;                                \
    ST_F ((void *)out, vd13, VL); OUT += VL;                                \
    ST_F ((void *)out, vd14, VL); OUT += VL;                                \
    ST_F ((void *)out, vd15, VL); OUT += VL;                                \

/* The widened destination register group of a dual widen ternary insn is tied
   to the accumulator, which is still live when the narrowed sources are read.
   Feeding one narrowed source from the highest-numbered half of the
   accumulator's own register group is thus the only way the sources can
   legally overlap the destination register group: the shared registers hold
   one single value, seen as the accumulator's high part and as the narrowed
   source at the same time.  RI_F reinterprets the widened type as the
   narrowed element type, GET_F extracts the highest-numbered half of it.  */
#define LOOP_DUAL_WIDEN_TERNARY_BODY_OVERLAP_X2(NT, WT, WNT, LD_NF, LD_WF,   \
						 RI_F, GET_F, OUT_F, ST_F,  \
						 OUT, START, VL)            \
    NT vt0 = LD_NF ((void *)START, VL); START += VL;                        \
    NT vt1 = LD_NF ((void *)START, VL); START += VL;                        \
    WT vw0 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw1 = LD_WF ((void *)START, VL); START += VL;                        \
                                                                            \
    asm volatile("nop" ::: "memory");                                       \
                                                                            \
    WNT vr0 = RI_F (vw0);                                                   \
    WNT vr1 = RI_F (vw1);                                                   \
                                                                            \
    NT vs0 = GET_F (vr0, 1);                                                \
    NT vs1 = GET_F (vr1, 1);                                                \
                                                                            \
    WT vd0 = OUT_F (vw0, vs0, vt0, VL);                                     \
    WT vd1 = OUT_F (vw1, vs1, vt1, VL);                                     \
                                                                            \
    asm volatile("nop" ::: "memory");                                       \
                                                                            \
    ST_F ((void *)out, vd0, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd1, VL); OUT += VL;                                 \

/* Like LOOP_DUAL_WIDEN_TERNARY_BODY_OVERLAP_X2 but the multiplier comes from
   a scalar register, thus the narrowed source is the only vector operand that
   can overlap the destination register group.  RI_F reinterprets the widened
   accumulator as the narrowed element type and GET_F extracts the
   highest-numbered half of it.  */
#define LOOP_DUAL_WIDEN_TERNARY_VX_BODY_OVERLAP_X2(NT, WT, WNT, LD_WF,      \
						   RI_F, GET_F, OUT_F,      \
						   ST_F, OUT, START, X, VL) \
    WT vw0 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw1 = LD_WF ((void *)START, VL); START += VL;                        \
                                                                            \
    asm volatile("nop" ::: "memory");                                       \
                                                                            \
    WNT vr0 = RI_F (vw0);                                                   \
    WNT vr1 = RI_F (vw1);                                                   \
                                                                            \
    NT vs0 = GET_F (vr0, 1);                                                \
    NT vs1 = GET_F (vr1, 1);                                                \
                                                                            \
    WT vd0 = OUT_F (vw0, X, vs0, VL);                                       \
    WT vd1 = OUT_F (vw1, X, vs1, VL);                                       \
                                                                            \
    asm volatile("nop" ::: "memory");                                       \
                                                                            \
    ST_F ((void *)out, vd0, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd1, VL); OUT += VL;                                 \

/* Like LOOP_DUAL_WIDEN_TERNARY_BODY_OVERLAP_X2 but for the mixed signed and
   unsigned narrowed sources.  The first insn overlaps the destination register
   group with the signed source, the second one with the unsigned source, thus
   both narrowed operands are covered.  RI_F reinterprets the widened
   accumulator as the signed narrowed element type, RI_UF and RI_NUF do the
   same for the unsigned narrowed element type, GET_F and GET_UF extract the
   highest-numbered half of it.  */
#define LOOP_DUAL_WIDEN_TERNARY_BODY_SU_OVERLAP_X2(NT, NUT, WT, WNT,        \
					    WNUT, LD_NF, LD_NUF, LD_WF,     \
					    RI_F, RI_UF, RI_NUF, GET_F,     \
					    GET_UF, OUT_F, ST_F, OUT,       \
					    START, VL)                      \
    NUT vt0 = LD_NUF ((void *)START, VL); START += VL;                      \
    NT vt1 = LD_NF ((void *)START, VL); START += VL;                        \
    WT vw0 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw1 = LD_WF ((void *)START, VL); START += VL;                        \
                                                                            \
    asm volatile("nop" ::: "memory");                                       \
                                                                            \
    WNT vr0 = RI_F (vw0);                                                   \
    WNUT vr1 = RI_NUF (RI_UF (vw1));                                        \
                                                                            \
    NT vs0 = GET_F (vr0, 1);                                                \
    NUT vs1 = GET_UF (vr1, 1);                                              \
                                                                            \
    WT vd0 = OUT_F (vw0, vs0, vt0, VL);                                     \
    WT vd1 = OUT_F (vw1, vt1, vs1, VL);                                     \
                                                                            \
    asm volatile("nop" ::: "memory");                                       \
                                                                            \
    ST_F ((void *)out, vd0, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd1, VL); OUT += VL;                                 \

/* Like LOOP_DUAL_WIDEN_TERNARY_VX_BODY_OVERLAP_X2 but the narrowed vector
   source is unsigned while the accumulator is signed, thus reinterpreting the
   accumulator takes two steps: RI_UF turns the signed widened accumulator into
   the unsigned widened type, RI_NUF reinterprets that as the unsigned narrowed
   element type and GET_UF extracts the highest-numbered half of it.  */
#define LOOP_DUAL_WIDEN_TERNARY_VX_BODY_SU_OVERLAP_X2(NUT, WT, WNUT, LD_WF, \
						      RI_UF, RI_NUF, GET_UF,\
						      OUT_F, ST_F, OUT,     \
						      START, X, VL)         \
    WT vw0 = LD_WF ((void *)START, VL); START += VL;                        \
    WT vw1 = LD_WF ((void *)START, VL); START += VL;                        \
                                                                            \
    asm volatile("nop" ::: "memory");                                       \
                                                                            \
    WNUT vr0 = RI_NUF (RI_UF (vw0));                                        \
    WNUT vr1 = RI_NUF (RI_UF (vw1));                                        \
                                                                            \
    NUT vs0 = GET_UF (vr0, 1);                                              \
    NUT vs1 = GET_UF (vr1, 1);                                              \
                                                                            \
    WT vd0 = OUT_F (vw0, X, vs0, VL);                                       \
    WT vd1 = OUT_F (vw1, X, vs1, VL);                                       \
                                                                            \
    asm volatile("nop" ::: "memory");                                       \
                                                                            \
    ST_F ((void *)out, vd0, VL); OUT += VL;                                 \
    ST_F ((void *)out, vd1, VL); OUT += VL;                                 \

#define DEF_GROUP_OVERLAP_UNARY_0(VL_F, NT, WT, LD_F, OUT_F, ST_F, NAME, \
				  LOOP_BODY)                             \
  void test_group_overlap_##NAME##_##NT##_unary_0(uint8_t *data,         \
						 uint8_t *out,           \
						 size_t limit)           \
  {                                                                      \
    uint8_t *start = data;                                               \
    uint8_t *end = data + limit;                                         \
    size_t vl = VL_F ();                                                 \
                                                                         \
    while (start < end) {                                                \
      LOOP_BODY (NT, WT, LD_F, OUT_F, ST_F, out, start, vl);             \
    }                                                                    \
  }

#define DEF_GROUP_OVERLAP_BINARY_0(VL_F, NT, WT, LD_NF, LD_WF, OUT_F, ST_F, \
				   NAME, LOOP_BODY)                          \
  void test_group_overlap_##NAME##_##NT##_binary_0(uint8_t *data,           \
						   uint8_t *out,            \
						   size_t limit)            \
  {                                                                         \
    uint8_t *start = data;                                                  \
    uint8_t *end = data + limit;                                            \
    size_t vl = VL_F ();                                                    \
                                                                            \
    while (start < end) {                                                   \
      LOOP_BODY (NT, WT, LD_NF, LD_WF, OUT_F, ST_F, out, start, vl);         \
    }                                                                       \
  }

#define DEF_GROUP_OVERLAP_BINARY_1(VL_F, NT, WT, LD_NF, OUT_F, ST_F, NAME,  \
				   LOOP_BODY)                               \
  void test_group_overlap_##NAME##_##NT##_binary_1(uint8_t *data,           \
						   uint8_t *out,            \
						   size_t limit)            \
  {                                                                         \
    uint8_t *start = data;                                                  \
    uint8_t *end = data + limit;                                            \
    size_t vl = VL_F ();                                                    \
                                                                            \
    while (start < end) {                                                   \
      LOOP_BODY (NT, WT, LD_NF, OUT_F, ST_F, out, start, vl);               \
    }                                                                       \
  }

#define DEF_GROUP_OVERLAP_BINARY_2(VL_F, NT, NUT, WT, LD_NF, LD_NUF,        \
				   OUT_F, ST_F, NAME, LOOP_BODY)            \
  void test_group_overlap_##NAME##_##NT##_binary_2(uint8_t *data,           \
						   uint8_t *out,            \
						   size_t limit)            \
  {                                                                         \
    uint8_t *start = data;                                                  \
    uint8_t *end = data + limit;                                            \
    size_t vl = VL_F ();                                                    \
                                                                            \
    while (start < end) {                                                   \
      LOOP_BODY (NT, NUT, WT, LD_NF, LD_NUF, OUT_F, ST_F, out, start,       \
		 vl);                                                       \
    }                                                                       \
  }

#define DEF_GROUP_OVERLAP_BINARY_3(VL_F, NT, WT, ST, LD_NF, OUT_F, ST_F,    \
				   NAME, LOOP_BODY)                         \
  void test_group_overlap_##NAME##_##NT##_binary_3(uint8_t *data,           \
						   uint8_t *out,            \
						   ST x,                    \
						   size_t limit)            \
  {                                                                         \
    uint8_t *start = data;                                                  \
    uint8_t *end = data + limit;                                            \
    size_t vl = VL_F ();                                                    \
                                                                            \
    while (start < end) {                                                   \
      LOOP_BODY (NT, WT, LD_NF, OUT_F, ST_F, out, start, x, vl);            \
    }                                                                       \
  }

#define DEF_GROUP_OVERLAP_TERNARY_0(VL_F, NT, WT, LD_NF, LD_WF, OUT_F,      \
				    ST_F, NAME, LOOP_BODY)                  \
  void test_group_overlap_##NAME##_##NT##_ternary_0(uint8_t *data,          \
						    uint8_t *out,           \
						    size_t limit)           \
  {                                                                         \
    uint8_t *start = data;                                                  \
    uint8_t *end = data + limit;                                            \
    size_t vl = VL_F ();                                                    \
                                                                            \
    while (start < end) {                                                   \
      LOOP_BODY (NT, WT, LD_NF, LD_WF, OUT_F, ST_F, out, start, vl);        \
    }                                                                       \
  }

#define DEF_GROUP_OVERLAP_TERNARY_1(VL_F, NT, WT, WNT, LD_NF, LD_WF, RI_F,  \
				    GET_F, OUT_F, ST_F, NAME, LOOP_BODY)    \
  void test_group_overlap_##NAME##_##NT##_ternary_1(uint8_t *data,          \
						    uint8_t *out,           \
						    size_t limit)           \
  {                                                                         \
    uint8_t *start = data;                                                  \
    uint8_t *end = data + limit;                                            \
    size_t vl = VL_F ();                                                    \
                                                                            \
    while (start < end) {                                                   \
      LOOP_BODY (NT, WT, WNT, LD_NF, LD_WF, RI_F, GET_F, OUT_F, ST_F, out,  \
		 start, vl);                                                \
    }                                                                       \
  }

#define DEF_GROUP_OVERLAP_TERNARY_2(VL_F, NT, NUT, WT, LD_NF, LD_NUF,       \
				    LD_WF, OUT_F, ST_F, NAME, LOOP_BODY)    \
  void test_group_overlap_##NAME##_##NT##_ternary_2(uint8_t *data,          \
						    uint8_t *out,           \
						    size_t limit)           \
  {                                                                         \
    uint8_t *start = data;                                                  \
    uint8_t *end = data + limit;                                            \
    size_t vl = VL_F ();                                                    \
                                                                            \
    while (start < end) {                                                   \
      LOOP_BODY (NT, NUT, WT, LD_NF, LD_NUF, LD_WF, OUT_F, ST_F, out,       \
		 start, vl);                                                \
    }                                                                       \
  }

#define DEF_GROUP_OVERLAP_TERNARY_3(VL_F, NT, NUT, WT, WNT, WNUT, LD_NF,    \
				    LD_NUF, LD_WF, RI_F, RI_UF, RI_NUF,     \
				    GET_F, GET_UF, OUT_F, ST_F, NAME,       \
				    LOOP_BODY)                              \
  void test_group_overlap_##NAME##_##NT##_ternary_3(uint8_t *data,          \
						    uint8_t *out,           \
						    size_t limit)           \
  {                                                                         \
    uint8_t *start = data;                                                  \
    uint8_t *end = data + limit;                                            \
    size_t vl = VL_F ();                                                    \
                                                                            \
    while (start < end) {                                                   \
      LOOP_BODY (NT, NUT, WT, WNT, WNUT, LD_NF, LD_NUF, LD_WF, RI_F,        \
		 RI_UF, RI_NUF, GET_F, GET_UF, OUT_F, ST_F, out, start, vl);\
    }                                                                       \
  }

#define DEF_GROUP_OVERLAP_TERNARY_4(VL_F, NT, WT, ST, LD_NF, LD_WF, OUT_F,  \
				    ST_F, NAME, LOOP_BODY)                  \
  void test_group_overlap_##NAME##_##NT##_ternary_4(uint8_t *data,          \
						    uint8_t *out,           \
						    ST x,                   \
						    size_t limit)           \
  {                                                                         \
    uint8_t *start = data;                                                  \
    uint8_t *end = data + limit;                                            \
    size_t vl = VL_F ();                                                    \
                                                                            \
    while (start < end) {                                                   \
      LOOP_BODY (NT, WT, LD_NF, LD_WF, OUT_F, ST_F, out, start, x, vl);     \
    }                                                                       \
  }

#define DEF_GROUP_OVERLAP_TERNARY_5(VL_F, NT, WT, WNT, ST, LD_WF, RI_F,     \
				    GET_F, OUT_F, ST_F, NAME, LOOP_BODY)    \
  void test_group_overlap_##NAME##_##NT##_ternary_5(uint8_t *data,          \
						    uint8_t *out,           \
						    ST x,                   \
						    size_t limit)           \
  {                                                                         \
    uint8_t *start = data;                                                  \
    uint8_t *end = data + limit;                                            \
    size_t vl = VL_F ();                                                    \
                                                                            \
    while (start < end) {                                                   \
      LOOP_BODY (NT, WT, WNT, LD_WF, RI_F, GET_F, OUT_F, ST_F, out, start,  \
		 x, vl);                                                    \
    }                                                                       \
  }

#define DEF_GROUP_OVERLAP_TERNARY_6(VL_F, NUT, WT, WNUT, ST, LD_WF, RI_UF,  \
				    RI_NUF, GET_UF, OUT_F, ST_F, NAME,      \
				    LOOP_BODY)                              \
  void test_group_overlap_##NAME##_##NUT##_ternary_6(uint8_t *data,         \
						     uint8_t *out,          \
						     ST x,                  \
						     size_t limit)          \
  {                                                                         \
    uint8_t *start = data;                                                  \
    uint8_t *end = data + limit;                                            \
    size_t vl = VL_F ();                                                    \
                                                                            \
    while (start < end) {                                                   \
      LOOP_BODY (NUT, WT, WNUT, LD_WF, RI_UF, RI_NUF, GET_UF, OUT_F, ST_F,  \
		 out, start, x, vl);                                        \
    }                                                                       \
  }

#endif
