/* PR target/65780 */
/* { dg-do link { target *-*-linux* *-*-gnu* *-*-uclinux* } } */
/* { dg-options "-O2" } */

int optopt;

int
main ()
{
  optopt = 4;
  return 0;
}
