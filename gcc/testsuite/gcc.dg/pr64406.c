/* { dg-do compile } */
/* { dg-options "-O -ftree-loop-distribute-patterns -fno-tree-loop-ivcanon -fno-tree-loop-vectorize -ftree-vectorize" } */

unsigned in[72];

void bar (unsigned out[], unsigned ia[]);

void
foo ()
{
  int i;
  unsigned out[72], ia[8];
  for (i = 0; i < 8; i++)
    {
      out[i * 8] = in[i * 8] + 5;
      out[i * 8 + 1] = in[i * 8 + 1] + 6;
      out[i * 8 + 2] = in[i * 8 + 2] + 7;
      out[i * 8 + 3] = in[i * 8 + 3] + 8;
      out[i * 8 + 4] = in[i * 8 + 4] + 9;
      out[i * 8 + 5] = in[i * 8 + 5] + 10;
      out[i * 8 + 6] = in[i * 8 + 6] + 11;
      out[i * 8 + 7] = in[i * 8 + 7] + 12;
      ia[i] = in[i];
    }
  bar (out, ia);
}
