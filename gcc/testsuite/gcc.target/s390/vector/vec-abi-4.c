/* Check calling convention in the vector ABI.  Smaller vector need to
   be placed left-justified in the stack slot.  */

/* { dg-do compile { target { s390*-*-* } } } */
/* { dg-options "-O3 -mzarch -march=z13" } */

/* { dg-final { scan-assembler-times "lde\t%.*,160\\\(%r15\\\)" 1 { target lp64 } } } */
/* { dg-final { scan-assembler-times "lde\t%.*,168\\\(%r15\\\)" 1 { target lp64 } } } */
/* { dg-final { scan-assembler-times "lde\t%.*,96\\\(%r15\\\)" 1 { target { ! lp64 } } } } */
/* { dg-final { scan-assembler-times "lde\t%.*,100\\\(%r15\\\)" 1 { target { ! lp64 } } } } */

typedef char __attribute__((vector_size(4))) v4qi;

v4qi
foo (v4qi a, v4qi b, v4qi c, v4qi d, v4qi e,
     v4qi f, v4qi g, v4qi h, v4qi i, v4qi j)
{
  return (a + b + c + d + e + f + g + h + i + j);
}
