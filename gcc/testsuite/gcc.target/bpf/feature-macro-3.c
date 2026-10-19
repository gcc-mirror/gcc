/* { dg-do compile } */
/* { dg-options "-mcpu=v1" } */

#ifndef __BPF_FEATURE_CALLX
#error __BPF_FEATURE_CALLX undefined with -mcpu=v1
#endif
