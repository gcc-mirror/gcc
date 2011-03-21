/* { dg-do run } */

extern void abort (void);
void
foo (void *p_)
{
  int *p;
  int i;
  for (i = 0; i < sizeof(int *); ++i)
    ((char *)&p)[i] = ((char *)p_)[i];
  *p = 1;
}
int main()
{
  int i = 0;
  int *p = &i;
  foo (&p);
  if (i != 1)
    abort ();
  return 0;
}
