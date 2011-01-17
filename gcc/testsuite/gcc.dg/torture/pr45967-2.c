/* { dg-do run } */

extern void abort (void);
int b;
void
foo (void *p_, int *q)
{
  int *p;
  int i;
  for (i = 0; i < sizeof(int *); ++i)
    ((char *)&p)[i] = ((char *)p_)[i];
  if (b)
    p = q;
  *p = 1;
}
int main()
{
  int i = 0, j;
  int *p = &i;
  foo (&p, &j);
  if (i != 1)
    abort ();
  return 0;
}
