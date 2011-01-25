/* { dg-do compile } */ 
/* { dg-options "-O2 -ftree-loop-linear" } */

int buf[256 * 9];
int f() 
{
  int i, j;

  for (i = 0; i < 256; ++i)
    for (j = 0; j < 8; ++j)
      buf[j + 1] = buf[j] + 1;

  return buf[10];
}
