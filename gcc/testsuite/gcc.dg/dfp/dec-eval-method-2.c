/* { dg-do compile } */
/* { dg-options "-std=gnu99 -D__STDC_WANT_DEC_FP__" } */
                                                                                
/* N1107 4: Characteristics of decimal floating types <float.h>.
   C99 5.2.4.2.2a[2] (New).

   Verify that DEC_EVAL_METHOD is defined by float.h.
   DEC_EVAL_METHOD in <float.h>.  */

#ifdef DEC_EVAL_METHOD
#error DEC_EVAL_METHOD is defined before float.h is included
#endif

#include <float.h>

#ifndef DEC_EVAL_METHOD
#error DEC_EVAL_METHOD is not defined after float.h is included
#endif

int i;
