/* { dg-do compile } */
/* { dg-require-effective-target fpic } */
/* { dg-options "-O2 -fpic -ftls-model=global-dynamic" } */
/* { dg-require-effective-target tls } */
/* { dg-skip-if "" { arc*-*-elf* } } */

/* Check if tls global dynamic is correctly generated.  */

extern __thread int e2;

int *ae2 (void)
{
  return &e2;
}

/* { dg-final { scan-assembler "add\\s+r0,pcl,@e2@tlsgd" } } */
/* { dg-final { scan-assembler "bl\\s+@__tls_get_addr@plt" } } */
