/* { dg-do compile } */
/* { dg-options "-mcpu=v1 -malu32 -mjmp32 -mjmpext -mbswap -msdiv -msmov" } */

#ifndef __BPF_FEATURE_ALU32
#error __BPF_FEATURE_ALU32 undefined
#endif

#ifndef __BPF_FEATURE_JMP32
#error __BPF_FEATURE_JMP32 undefined
#endif

#ifndef __BPF_FEATURE_JMP_EXT
#error __BPF_FEATURE_JMP_EXT undefined
#endif

#ifndef __BPF_FEATURE_SDIV_SMOD
#error __BPF_FEATURE_SDIV_SMOD undefined
#endif

#ifndef __BPF_FEATURE_MOVSX
#error __BPF_FEATURE_MOVSX undefined
#endif

#ifdef __BPF_FEATURE_LDSX
#error __BPF_FEATURE_LDSX defined with -mcpu=v1
#endif

#ifdef __BPF_FEATURE_GOTOL
#error __BPF_FEATURE_GOTOL defined with -mcpu=v4
#endif

#ifdef __BPF_FEATURE_ST
#error __BPF_FEATURE_ST defined with -mcpu=v4
#endif
