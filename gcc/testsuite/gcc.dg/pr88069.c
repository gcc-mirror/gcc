/* { dg-do compile } */
/* { dg-options "-O -ftree-pre -ftree-vectorize -fno-tree-pta" } */

void
qf (void);

void
mr (short int db)
{
  int vq;
  short int *lp = &db;

  for (vq = 0; vq < 1; ++vq)
    qf ();

  while (*lp < 2)
    {
      *lp = db;
      lp = (short int *) &vq;
      ++*lp;
    }
}

