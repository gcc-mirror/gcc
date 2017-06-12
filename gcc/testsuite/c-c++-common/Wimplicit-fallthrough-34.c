/* PR c/77946 */
/* { dg-do compile } */
/* { dg-require-effective-target indirect_jumps } */
/* { dg-options "-Wimplicit-fallthrough" } */

void
foo (void)
{
  static void *p = &&lab;
  goto *p;
  /*FALLTHRU*/
  lab:;
}
