/* { dg-do run } */
/* { dg-options "-O2" } */

extern void abort (void);
struct S {
    int *i[4];
    int *p1;
    int *p2;
    int *p3;
    int *p4;
};
int **b;
int main()
{
  int i = 1;
  struct S s;
  s.p3 = &i;
  int **p;
  if (b)
    p = b;
  else
    p = &s.i[2];
  p += 4;
  if (!b)
    **p = 0;
  if (i != 0)
    abort ();
  return i;
}
