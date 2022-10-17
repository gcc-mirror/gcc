/* { dg-do compile } */
/* { dg-require-effective-target fpic } */
/* { dg-options "-O2 -fpic -ftls-model=initial-exec" } */
/* { dg-require-effective-target tls } */
/* { dg-skip-if "" { arc*-*-elf* } } */

/* Check if tls initial execution is correctly generated.  */

extern __thread int e2;

int *ae2 (void)
{
  return &e2;
}

/* { dg-final { scan-assembler "ld\\s+r0,\\\[pcl,@e2@tlsie\\\]" } } */
/* { dg-final { scan-assembler "add_s\\s+r0,r0,r25" } } */
