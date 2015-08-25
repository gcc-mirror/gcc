/* PR middle-end/67330 */
/* { dg-do compile } */
/* { dg-require-weak "" } */

void
f (void)
{
  __attribute__ ((weak)) int a; /* { dg-error "weak declaration of .a. must be public" } */
}
