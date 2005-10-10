/* PR target/24284 */
/* { dg-do compile { target sparc*-*-* } } */
/* { dg-options "-O -g" } */

void do_run(void *ip)
{
  char dummy[8192];

  __asm__("" : : "g"(dummy));

  goto *ip;
}
