/* Check calling convention in the vector ABI.  */

/* { dg-do compile { target { s390*-*-* } } } */
/* { dg-options "-O3 -mzarch -march=z13" } */

/* c.i and c.j are passed by reference since a struct with two
   elements is no vector type argument.  */
/* { dg-final { scan-assembler "ld\t%v\[0-9\]*,0\\(%r3\\)" } } */
/* { dg-final { scan-assembler "ld\t%v\[0-9\]*,8\\(%r3\\)" } } */

/* just_v2si is passed in a vector reg if it as an incoming arg.
   However, as return value it is passed via hidden first pointer
   argument.  */
/* { dg-final { scan-assembler ".*st.*\t%v\[0-9\]*,0\\(%r2\\)" } } */

/* { dg-final { scan-assembler "gnu_attribute 8, 2" } } */

typedef int __attribute__ ((vector_size(8))) v2si;

struct just_v2si
{
  v2si i;
};

struct two_v2si
{
  v2si i, j;
};

struct just_v2si
add_structvecs (v2si a, struct just_v2si b, struct two_v2si c)
{
  struct just_v2si res;

  res.i = a + b.i + c.i + c.j;
  return res;
}
