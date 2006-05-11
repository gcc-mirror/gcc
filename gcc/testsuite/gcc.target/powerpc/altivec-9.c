/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec -mabi=altivec -g" } */

/* PR9564 */

extern int vfork(void);

void
boom (void)
{
  char buf[65536];
  vfork();
}
