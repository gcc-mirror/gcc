/* PR target/65780 */
/* { dg-do link { target *-*-linux* *-*-gnu* } } */
/* { dg-require-effective-target pie } */
/* { dg-options "-O2 -fpie" } */

int optopt;

int
main ()
{
  optopt = 4;
  return 0;
}
