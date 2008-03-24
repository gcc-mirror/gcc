/* { dg-do compile } */
/* { dg-options "-fno-tree-pre -fno-tree-loop-im" } */

void putShort (int, int);

int t2;
void f(int t1)
{
  int clutOffset = 52 + 256 * 3 * 2;
  int x, y, z;
  for (x = 0; x < 16; x++)
    for (y = 0; y < 16; y++)
      for (z = 0; z < 16; z++)
        {
          int offset = clutOffset + z * 6 + y * 16 * 6 + x * 16 * 16 * 6;
          double xf = ((double) x) / ((double) 16 - 1.0);
          double tt = xf;
          putShort(offset, tt);
        } 
}

