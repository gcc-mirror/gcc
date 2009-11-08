/* PR target/41985 */
/* { dg-do compile } */
/* { dg-options "" } */

int
main ()
{
  int i;
  asm volatile ("# %&": : "g" (i));	/* { dg-error "used without any local dynamic TLS references" } */
  return 0;
}
