/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

extern struct {
  int o[2];
  int p[2];
} d;

void C()
{
  int i;

  for( i=0; i<2; ++i )
    {
      d.o[i] = 0;
      d.p[i] = 0;
    }
  return;
}
