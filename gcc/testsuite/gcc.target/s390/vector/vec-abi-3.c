/* Check calling convention in the vector ABI regarding vector like structs.  */

/* { dg-do compile { target { s390*-*-* } } } */
/* { dg-options "-O3 -mzarch -march=z13" } */

/* addA */
/* { dg-final { scan-assembler-times "vfadb\t%v24,%v24,%v26" 1 } } */

/* addB and addE*/
/* { dg-final { scan-assembler-times "vah\t%v24,%v\[0-9\]*,%v\[0-9\]*" 2 } } */

/* addC */
/* { dg-final { scan-assembler-times "vag\t%v24,%v\[0-9\]*,%v\[0-9\]*" 1 } } */

/* addB and addC are expected to read the arguments via pointers in r2 and r3 */
/* { dg-final { scan-assembler-times "vl\t%v\[0-9\]*,0\\(%r2\\)" 2 } } */
/* { dg-final { scan-assembler-times "vl\t%v\[0-9\]*,0\\(%r3\\)" 2 } } */

/* addD */
/* { dg-final { scan-assembler-times "vaf\t%v24,%v24,%v26" 1 } } */

/* addE */
/* { dg-final { scan-assembler-times "vah\t%v24,%v24,%v26" 1 } } */

/* addF */
/* { dg-final { scan-assembler-times "vab\t%v24,%v\[0-9\]*,%v\[0-9\]*" 1 } } */
/* { dg-final { scan-assembler-times "srlg\t%r\[0-9\]*,%r2,32" 1 { target lp64 } } } */
/* { dg-final { scan-assembler-times "srlg\t%r\[0-9\]*,%r3,32" 1 { target lp64 } } } */
/* { dg-final { scan-assembler-times "llgfr\t%.*,%r2" 1 { target { ! lp64 } } } } */
/* { dg-final { scan-assembler-times "llgfr\t%.*,%r4" 1 { target { ! lp64 } } } } */


typedef double v2df __attribute__((vector_size(16)));
typedef long long v2di __attribute__((vector_size(16)));
typedef int v4si __attribute__((vector_size(16)));
typedef short v8hi __attribute__((vector_size(16)));

typedef short v2hi __attribute__((vector_size(4)));
typedef char v4qi __attribute__((vector_size(4)));

/* Vector like structs are passed in VRs.  */
struct A { v2df a; };

v2df
addA (struct A a, struct A b)
{
  return a.a + b.a;
}

/* Only single element vectors qualify as vector type parms.  This one
   is passed as a struct. Since it is bigger than 8 bytes it is passed
   on the stack with the reference being put into r2/r3.  */
struct B { v8hi a; char b;};

v8hi
addB (struct B a, struct B b)
{
  return a.a + b.a;
}

/* The resulting struct is bigger than 16 bytes and therefore passed
   on the stack with the references residing in r2/r3.  */
struct C { v2di __attribute__((aligned(32))) a; };

v2di
addC (struct C a, struct C b)
{
  return a.a + b.a;
}

/* The attribute here does not have any effect. So this struct stays
   vector like and hence is passed in a VR.  */
struct D { v4si __attribute__((aligned(16))) a; };

v4si
addD (struct D a, struct D b)
{
  return a.a + b.a;
}


/* Smaller vectors are passed in vector registers. This also applies
   for vector like structs.  */
struct E { v2hi a; };

v2hi
addE (struct E a, struct E b)
{
  return a.a + b.a;
}

/* This struct is not passed in VRs because of padding.  But since it
   fits in a GPR and has a power of two size. It is passed in
   GPRs.  */
struct F { v4qi __attribute__((aligned(8))) a; };

v4qi
addF (struct F a, struct F b)
{
  return a.a + b.a;
}
