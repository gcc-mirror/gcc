/* { dg-do compile } */
/* { dg-options "-O3 -fno-inline" } */

/* Scan-assembler test, so, incorporate as little other code as possible.  */

#include "arm_neon.h"
#include "int_comparisons.x"

/* Operations on all 18 integer types:  (q?)_[su](8|16|32|64), d_[su]64.
   (d?)_[us]64 generate regs of form 'd0' rather than e.g. 'v0.2d'.  */
/* { dg-final { scan-assembler-times "\[ \t\]cmeq\[ \t\]+v\[0-9\]+\.\[0-9\]+\[bshd\],\[ \t\]*v\[0-9\]+\.\[0-9\]+\[bshd\],\[ \t\]*#?0" 14 } } */
/* { dg-final { scan-assembler-times "\[ \t\]cmeq\[ \t\]+d\[0-9\]+,\[ \t\]*d\[0-9\]+,\[ \t\]*#?0" 4 } } */
/* { dg-final { scan-assembler-times "\[ \t\]cmeq\[ \t\]+v\[0-9\]+\.\[0-9\]+\[bshd\],\[ \t\]*v\[0-9\]+\.\[0-9\]+\[bshd\],\[ \t\]+v\[0-9\]+\.\[0-9\]+\[bshd\]" 14 } } */
/* { dg-final { scan-assembler-times "\[ \t\]cmeq\[ \t\]+d\[0-9\]+,\[ \t\]*d\[0-9\]+,\[ \t\]+d\[0-9\]+" 4 } } */
/* { dg-final { scan-assembler-times "\[ \t\]cmtst\[ \t\]+v\[0-9\]+\.\[0-9\]+\[bshd\],\[ \t\]*v\[0-9\]+\.\[0-9\]+\[bshd\],\[ \t\]+v\[0-9\]+\.\[0-9\]+\[bshd\]" 14 } } */
/* { dg-final { scan-assembler-times "\[ \t\]cmtst\[ \t\]+d\[0-9\]+,\[ \t\]*d\[0-9\]+,\[ \t\]+d\[0-9\]+" 4 } } */

/* vcge + vcle both implemented with cmge (signed) or cmhs (unsigned).  */
/* { dg-final { scan-assembler-times "\[ \t\]cmge\[ \t\]+v\[0-9\]+\.\[0-9\]+\[bshd\],\[ \t\]*v\[0-9\]+\.\[0-9\]+\[bshd\],\[ \t\]+v\[0-9\]+\.\[0-9\]+\[bshd\]" 14 } } */
/* { dg-final { scan-assembler-times "\[ \t\]cmge\[ \t\]+d\[0-9\]+,\[ \t\]*d\[0-9\]+,\[ \t\]+d\[0-9\]+" 4 } } */
/* { dg-final { scan-assembler-times "\[ \t\]cmhs\[ \t\]+v\[0-9\]+\.\[0-9\]+\[bshd\],\[ \t\]*v\[0-9\]+\.\[0-9\]+\[bshd\],\[ \t\]+v\[0-9\]+\.\[0-9\]+\[bshd\]" 14 } } */
/* { dg-final { scan-assembler-times "\[ \t\]cmhs\[ \t\]+d\[0-9\]+,\[ \t\]*d\[0-9\]+,\[ \t\]+d\[0-9\]+" 4 } } */

/* vcgt + vclt both implemented with cmgt (signed) or cmhi (unsigned).  */
/* { dg-final { scan-assembler-times "\[ \t\]cmgt\[ \t\]+v\[0-9\]+\.\[0-9\]+\[bshd\],\[ \t\]*v\[0-9\]+\.\[0-9\]+\[bshd\],\[ \t\]+v\[0-9\]+\.\[0-9\]+\[bshd\]" 14 } } */
/* { dg-final { scan-assembler-times "\[ \t\]cmgt\[ \t\]+d\[0-9\]+,\[ \t\]*d\[0-9\]+,\[ \t\]+d\[0-9\]+" 4 } } */
/* { dg-final { scan-assembler-times "\[ \t\]cmhi\[ \t\]+v\[0-9\]+\.\[0-9\]+\[bshd\],\[ \t\]*v\[0-9\]+\.\[0-9\]+\[bshd\],\[ \t\]+v\[0-9\]+\.\[0-9\]+\[bshd\]" 14 } } */
/* { dg-final { scan-assembler-times "\[ \t\]cmhi\[ \t\]+d\[0-9\]+,\[ \t\]*d\[0-9\]+,\[ \t\]+d\[0-9\]+" 4 } } */

/* Comparisons against immediate zero, on the 8 signed integer types only.  */

/* { dg-final { scan-assembler-times "\[ \t\]cmge\[ \t\]+v\[0-9\]+\.\[0-9\]+\[bshd\],\[ \t\]*v\[0-9\]+\.\[0-9\]+\[bshd\],\[ \t\]*#?0" 7 } } */
/* { dg-final { scan-assembler-times "\[ \t\]cmge\[ \t\]+d\[0-9\]+,\[ \t\]*d\[0-9\]+,\[ \t\]*#?0" 2 } } */
/* { dg-final { scan-assembler-times "\[ \t\]cmgt\[ \t\]+v\[0-9\]+\.\[0-9\]+\[bshd\],\[ \t\]*v\[0-9\]+\.\[0-9\]+\[bshd\],\[ \t\]*#?0" 7 } } */
/* { dg-final { scan-assembler-times "\[ \t\]cmgt\[ \t\]+d\[0-9\]+,\[ \t\]*d\[0-9\]+,\[ \t\]*#?0" 2 } } */
/* { dg-final { scan-assembler-times "\[ \t\]cmle\[ \t\]+v\[0-9\]+\.\[0-9\]+\[bshd\],\[ \t\]*v\[0-9\]+\.\[0-9\]+\[bshd\],\[ \t\]*#?0" 7 } } */
/* { dg-final { scan-assembler-times "\[ \t\]cmle\[ \t\]+d\[0-9\]+,\[ \t\]*d\[0-9\]+,\[ \t\]*#?0" 2 } } */
/* { dg-final { scan-assembler-times "\[ \t\]cmlt\[ \t\]+v\[0-9\]+\.\[0-9\]+\[bshd\],\[ \t\]*v\[0-9\]+\.\[0-9\]+\[bshd\],\[ \t\]*#?0" 7 } } */
/* For int64_t and int64x1_t, cmlt ... #0 and sshr ... #63 are equivalent,
   so allow either.  */
/* { dg-final { scan-assembler-times "\[ \t\](?:cmlt|sshr)\[ \t\]+d\[0-9\]+,\[ \t\]*d\[0-9\]+,\[ \t\]*#?(?:0|63)" 2 } } */

// All should have been compiled into single insns without inverting result:
/* { dg-final { scan-assembler-not "\[ \t\]not\[ \t\]" } } */
/* { dg-final { scan-assembler-not "\[ \t\]mvn\[ \t\]" } } */
