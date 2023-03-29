/* { dg-do compile { target { s390-*-* } } } */
/* { dg-options "-foptimize-sibling-calls -mesa" } */
/* { dg-final { scan-assembler {brasl\t%r\d+,bar} } } */

/* This tests function s390_call_saved_register_used where
   REG_P (parm_rtx) and nregs == 2 holds.  */

#include "pr106355.h"
