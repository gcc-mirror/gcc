/* { dg-do compile } */
/* { dg-options "-mcpu=v4" } */

#ifndef __BPF_FEATURE_LDSX
#error __BPF_FEATURE_LDSX undefined with -mcpu=v4
#endif

#ifndef __BPF_FEATURE_GOTOL
#error __BPF_FEATURE_GOTOL undefined with -mcpu=v4
#endif

#ifndef __BPF_FEATURE_ST
#error __BPF_FEATURE_ST undefined with -mcpu=v4
#endif
