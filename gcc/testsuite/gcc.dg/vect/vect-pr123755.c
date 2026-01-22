/* { dg-do compile } */

float a[4], b[4];

int
main ()
{
  int i;
  for (i = 0; i < 4; ++i)
    b[i] = __builtin_fma (1024.0f, 1024.0f, a[i]);
  return 0;
}
