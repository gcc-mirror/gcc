/* { dg-do compile } */
/* { dg-options "-O3 -ftree-vectorize" } */

int col[8];
int extend_options(int w, int h, int *map, int x, int y, int index)
{
  int dx, dy;
  for (dx = -1; dx <= +1; dx++)
    {
      int index = (dy < 0 ? 6-dx : dy > 0 ? 2+dx : 2*(1+dx));
      if (x+dx >= 0 && x+dx < w && y+dy >= 0 && y+dy < h)
        col[index] = map[(y+dy)*w+(x+dx)];
      col[index] = -1;
    }
}
