/* Check calling convention in the vector ABI.  */

/* { dg-do compile { target { s390*-*-* } } } */
/* { dg-options "-O3 -mzarch -march=z13" } */

/* Make sure the last argument is fetched from the argument overflow area.  */
/* { dg-final { scan-assembler "vl\t%v\[0-9\]*,160\\(%r15\\)" { target lp64 } } } */
/* { dg-final { scan-assembler "vl\t%v\[0-9\]*,96\\(%r15\\)" { target ilp32 } } } */
/* { dg-final { scan-assembler "gnu_attribute 8, 2" } } */

typedef double v2df __attribute__((vector_size(16)));

v2df
add (v2df a, v2df b, v2df c, v2df d,
     v2df e, v2df f, v2df g, v2df h, v2df i)
{
  return a + b + c + d + e + f + g + h + i;
}
