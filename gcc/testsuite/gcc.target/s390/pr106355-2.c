/* { dg-do compile { target { s390-*-* } } } */
/* { dg-options "-foptimize-sibling-calls -mzarch" } */
/* { dg-final { scan-assembler {brasl\t%r\d+,bar} } } */

/* This tests function s390_call_saved_register_used where
   GET_CODE (parm_rtx) == PARALLEL holds.  */

#include "pr106355.h"
