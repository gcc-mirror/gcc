/* Check calling convention with variable argument lists in the vector
   ABI.  */

/* { dg-do run { target { s390*-*-* } } } */
/* { dg-require-effective-target s390_vx } */
/* { dg-options "-O3 -mzarch -march=z13 --save-temps" } */

/* Make sure arguments are fetched from the argument overflow area.  */
/* { dg-final { scan-assembler "vl\t%v\[0-9\]*,352\\(%r15\\)" { target lp64 } } } */
/* { dg-final { scan-assembler "ld\t%v\[0-9\]*,368\\(%r15\\)" { target lp64 } } } */
/* { dg-final { scan-assembler "vl\t%v\[0-9\]*,376\\(%r15\\)" { target lp64 } } } */
/* { dg-final { scan-assembler "ld\t%v\[0-9\]*,392\\(%r15\\)" { target lp64 } } } */

/* { dg-final { scan-assembler "vl\t%v\[0-9\]*,208\\(%r15\\)" { target ilp32 } } } */
/* { dg-final { scan-assembler "ld\t%v\[0-9\]*,224\\(%r15\\)" { target ilp32 } } } */
/* { dg-final { scan-assembler "vl\t%v\[0-9\]*,232\\(%r15\\)" { target ilp32 } } } */
/* { dg-final { scan-assembler "ld\t%v\[0-9\]*,248\\(%r15\\)" { target ilp32 } } } */


#include <stdarg.h>

extern void abort (void);

typedef long long v2di __attribute__((vector_size(16)));
typedef int v2si __attribute__((vector_size(8)));

v2di __attribute__((noinline))
add (int a, ...)
{
  int i;
  va_list va;
  v2di di_result = { 0, 0 };
  v2si si_result = (v2si){ 0, 0 };

  va_start (va, a);

  di_result += va_arg (va, v2di);
  si_result += va_arg (va, v2si);
  di_result += va_arg (va, v2di);
  si_result += va_arg (va, v2si);

  va_end (va);

  di_result[0] += si_result[0];
  di_result[1] += si_result[1];

  return di_result;
}

int
main ()
{
  v2di r = add (4, (v2di){ 11, 21 }, (v2si){ 12, 22 }, (v2di){ 13, 23 }, (v2si){ 14, 24 });

  if (r[0] != 50 || r[1] != 90)
    abort ();

  return 0;
}
