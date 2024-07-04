/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-options "-maltivec -mabi=altivec -g" } */
/* { dg-require-effective-target powerpc_altivec } */

/* PR9564 */

extern int vfork(void);

void
boom (void)
{
  char buf[65536];
  vfork();
}
