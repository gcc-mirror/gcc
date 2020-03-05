/* PR target/90547 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

void
foo ()
{
  void *g[] = {&&a, &&b};

  for (unsigned c = 0x1F;; c >>= 1)
    {
      unsigned d = (long)("a"+1);
      long e = 8;

      while (e)
        {
          a: goto *g[c&d];
          b: e--;
        }
    }
}
