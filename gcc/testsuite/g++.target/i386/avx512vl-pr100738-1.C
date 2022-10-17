/* { dg-do compile } */
/* { dg-options "-Ofast -march=cascadelake" } */
/* { dg-final {scan-assembler-times "vblendvps\[ \\t\]" 2 } } */
/* { dg-final {scan-assembler-not "vpcmpeqd\[ \\t\]" } } */
/* { dg-final {scan-assembler-not "vpxor\[ \\t\]" } } */
/* { dg-final {scan-assembler-not "vpternlogd\[ \\t\]" } } */

#include "pr100738-1.C"
