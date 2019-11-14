/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <arm_sve.h>

/* Make sure that RDFFR comes after the LDFF1 and that the RDFFRs can
   be CSEd.  */
svint8_t
foo (svbool_t pg, int8_t *__restrict ptr,
     svbool_t *__restrict *__restrict preds)
{
  svsetffr ();
  svint8_t x = svldff1 (pg, ptr);
  *preds[0] = svrdffr ();
  *preds[1] = svrdffr ();
  return x;
}

/* { dg-final { scan-assembler {\tsetffr\n.*\tldff1b\t.*\trdffr\t} } } */
/* { dg-final { scan-assembler-times {\trdffr\t} 1 } } */
