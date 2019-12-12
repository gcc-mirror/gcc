/* { dg-do compile } */
/* { dg-require-effective-target fpic } */
/* { dg-options "-O2 -fpic -ftls-model=local-exec" } */
/* { dg-require-effective-target tls } */
/* { dg-skip-if "" { arc*-*-elf* } } */

/* Check if tls local execution is correctly generated.  */

extern __thread int e2;

int *ae2 (void)
{
  return &e2;
}

/* { dg-final { scan-assembler "add r0,r25,@e2@tpoff" } } */
