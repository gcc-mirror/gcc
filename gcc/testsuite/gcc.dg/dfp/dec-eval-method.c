/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */
                                                                                
/* N1107 4: Characteristics of decimal floating types <float.h>.
   C99 5.2.4.2.2a[2] (New).

   Verify that __DEC_EVAL_METHOD__ is defined; it will be used to define
   DEC_EVAL_METHOD in <float.h>.  */

int i;
#ifndef __DEC_EVAL_METHOD__
#error __DEC_EVAL_METHOD__ is not defined
#endif
