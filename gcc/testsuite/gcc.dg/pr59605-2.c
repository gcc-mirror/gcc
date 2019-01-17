/* { dg-do run } */
/* { dg-options "-O2" } */
/* { dg-additional-options "-DMAX_COPY=1025" { target { { simulator } || { nvptx-*-* amdgcn*-*-* } } } } */
/* { dg-additional-options "-minline-stringops-dynamically" { target { i?86-*-* x86_64-*-* } } } */

#include "pr59605.c"
