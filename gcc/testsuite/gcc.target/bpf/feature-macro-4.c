/* { dg-do compile } */
/* { dg-options "-mno-callx" } */

#ifdef __BPF_FEATURE_CALLX
#error __BPF_FEATURE_CALLX defined with -mno-callx
#endif
