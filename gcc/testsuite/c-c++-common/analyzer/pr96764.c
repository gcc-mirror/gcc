/* { dg-additional-options "-Wno-analyzer-out-of-bounds" } */

void
ar (int *hd)
{
  int **zv = &hd;
  *(double *) zv = 0.0;
}
