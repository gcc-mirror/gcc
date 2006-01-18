/* { dg-do compile } */

int dequeue (float *a);
void work (int i, float *a);
void
a13 (float *x, float *y)
{
  int ix_next, iy_next;
#pragma omp parallel shared(x, y) private(ix_next, iy_next)
  {
#pragma omp critical (xaxis)
    ix_next = dequeue (x);
    work (ix_next, x);
#pragma omp critical (yaxis)
    iy_next = dequeue (y);
    work (iy_next, y);
  }
}
