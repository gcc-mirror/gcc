/* PR target/82145 */
/* { dg-do compile { target { pie && lp64 } } } */
/* { dg-options "-O2 -fpie -mcmodel=large -march=haswell" } */

int l;

int
main ()
{
  l++;
  return 0;
}
