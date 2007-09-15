/* { dg-do compile } */
void f (unsigned int *d, unsigned int *s, int w)
{
  int i;
  for (i = 0; i < w; ++i)
    d [i] = s [i] * (unsigned short) (~d [i] >> 24);
}
/* { dg-final { cleanup-tree-dump "vect" } } */
