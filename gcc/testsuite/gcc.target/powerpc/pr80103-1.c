/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mpower9-dform-vector -mno-direct-move" } */
/* { dg-excess-errors "expect error due to conflicting target options" } */
/* Since the error message is not associated with a particular line
   number, we cannot use the dg-error directive and cannot specify a
   regexp to describe the expected error message.  The expected error
   message is: "-mpower9-dform, -mpower9-dform-vector,
                -mpower9-dform-scalar require -mdirect-move" */

int a;
void b (__attribute__ ((__vector_size__ (16))) char c)
{
  a = ((__attributes__ ((__vector_size__ (2 * sizeof (long)))) long) c)[0];
}
