/* PR target/89945 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

void
foo ()
{
  void *g[] = {&&a, &&b};

  for (unsigned c = 0x1F;; c >>= 1)
    {
      unsigned d = (long)"a";
      long e = 8;

      while (e)
        {
          a: goto *g[c&d];
          b: e--;
        }
    }
}
