/* PR target/81766 */
/* { dg-do compile { target { pie && lp64 } } } */
/* { dg-options "-O2 -fpie -mcmodel=large" } */

int
main ()
{
  return 0;
}
