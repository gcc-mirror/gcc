/* This testcase checks that s390_extzv_shift_ok does not cause an assertion
   failure.  */

/* { dg-do compile } */
/* { dg-options "-w -march=z196 -mtune=zEC12 -m64 -mzarch -O2" } */

int
foo (int a)
{
  return sizeof (int) * a + 16 - a * sizeof (int) % 16;
}
