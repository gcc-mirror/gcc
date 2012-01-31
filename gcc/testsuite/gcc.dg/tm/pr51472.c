/* { dg-do compile } */
/* { dg-options "-fgnu-tm -O  --param tm-max-aggregate-size=32" } */
/* { dg-options "-fgnu-tm -fno-common -O  --param tm-max-aggregate-size=32" { target hppa*-*-hpux* } } */

typedef int __attribute__ ((vector_size (16))) vectype;
vectype v;

void
foo (int c)
{
  vectype *p = __builtin_malloc (sizeof (vectype));
  __transaction_atomic
  {
    *p = v;
    if (c)
      __transaction_cancel;
  }
}
