/* { dg-do compile } */

int x[4];
int j[4];

void
foo (void)
{
  x[0] = (x[0] << j[0]) + j[0];
  x[1] = (x[0] << j[0]) + j[1];
  x[2] = (x[2] << j[0]) + j[2];
  x[3] = (x[3] << j[0]) + j[3];
}
