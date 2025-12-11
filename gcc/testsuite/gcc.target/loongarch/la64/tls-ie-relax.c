/* { dg-do compile } */
/* { dg-options "-O2 -mcmodel=normal -mexplicit-relocs -mrelax" } */
/* { dg-final { scan-assembler-times "R_LARCH_RELAX" 2 { target tls_native } } } */

extern __thread int errno;

void
unimplemented (void)
{
  errno = -38;
}
