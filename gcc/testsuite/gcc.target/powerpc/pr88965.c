/* PR target/88965 */
/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O2 -mvsx" } */

unsigned int a[16];
unsigned int __attribute__ ((vector_size (16))) b;

void
foo (void)
{
  b = __builtin_vec_vsx_ld (0, &a[0]);
}

void
bar (void)
{
  __builtin_vec_vsx_st (b, 0, &a[0]);
}
