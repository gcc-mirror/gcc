/* PR ipa/83506 */
/* { dg-do compile { target pthread } } */
/* { dg-options "-O1 -ftree-parallelize-loops=2 -fno-ipa-pure-const" } */

unsigned int
foo (unsigned int x, int y)
{
  while (y < 1)
    {
      x *= 3;
      ++y;
    }
  return x;
}
