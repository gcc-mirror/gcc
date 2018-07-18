/* { dg-do compile { target { powerpc64*-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mcpu=power8 -O2" } */

/* Verify that we optimize vector1 = (vector2 != vector3) by not loading up
   0/-1.  */

vector unsigned char
test (vector unsigned char a, vector unsigned char b)
{
  return a != b;
}

/* { dg-final { scan-assembler     {\mvcmpequb\M} } } */
/* { dg-final { scan-assembler     {\mxxlnor\M}   } } */
/* { dg-final { scan-assembler-not {\mxxspltib\M} } } */
/* { dg-final { scan-assembler-not {\mvspltisw\M} } } */
/* { dg-final { scan-assembler-not {\mxxlxor\M}   } } */
/* { dg-final { scan-assembler-not {\mxxlxorc\M}  } } */
/* { dg-final { scan-assembler-not {\mxxsel\M}    } } */
