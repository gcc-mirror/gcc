/* { dg-do compile } */
/* { dg-additional-options "-fno-tree-ccp -fno-tree-forwprop" } */

void
iw (int gu, int mq, int r2)
{
  int yn = 0;

  while (gu < 1)
    {
      int ay = 0;

      for (;;)
        ;

 bb:
      while (ay < 1)
        ++mq;
    }

  if (yn != 0)
    goto up;

  if (0)
    {
 up:
      if (r2 == 0)
        goto bb;
    }

  goto up;
}
