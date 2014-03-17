/* { dg-do run } */
/* { dg-options "-O2" } */

extern void abort (void);
struct S {
    int *i[4];
    int *p1;
    int *p2;
    int *p3;
    int *p4;
    int **x;
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
  /* prevert fowrprop from creating an offsetted sd constraint and
     preserve the pointer offsetting constraint.  */
  s.x = p;
  p = s.x;
  if (!b)
    {
      int *z = *p;
      /* z should point to i (and non-local/escaped).  */
      *z = 0;
    }
  if (i != 0)
    abort ();
  return i;
}
