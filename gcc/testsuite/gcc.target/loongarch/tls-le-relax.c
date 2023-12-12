/* { dg-do compile } */
/* { dg-options "-O2 -mcmodel=normal -mexplicit-relocs" } */
/* { dg-final { scan-assembler "%le_add_r" { target tls_le_relax } } } */

__attribute__ ((tls_model ("local-exec"))) __thread int a;

void
test (void)
{
  a = 10;
}

