/* Ensure we don't ICE when tracking optimization record scopes within
   the vectorizer.  */
/* { dg-do compile } */
/* { dg-options "-O1 -fsave-optimization-record -ftree-vectorize -fno-tree-scev-cprop -fno-tree-sink" } */

void
fk (unsigned int sf)
{
  for (;;)
    {
      if (sf != 0)
        {
          while (sf != 0)
            ++sf;

          while (sf < 8)
            ++sf;
        }

      ++sf;
    }
}
