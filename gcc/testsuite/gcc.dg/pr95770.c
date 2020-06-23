/* { dg-do compile } */
/* { dg-options "-O3" } */

float *a;
void b(float c, float d)
{
  a[0] = a[1] = 0.5f * (c - 2 + d);
  a[2] = a[3] = 0.5f * (c + 2 + d);
}
