/*  PR rtl-optimization/16536
    Origin:  Jeremy Denise      <jeremy.denise@libertysurf.fr>
    Reduced: Wolfgang Bangerth  <bangerth@dealii.org>
             Volker Reichelt    <reichelt@igpm.rwth-aachen.de>  */
/* { dg-options "-fgnu89-inline" } */

extern void abort ();

typedef struct
{
  int i, dummy;
} A;

inline A foo (const A* p, const A* q)
{
  return (A){p->i+q->i};
}

void bar (A* __restrict__ p)
{
  *p=foo(p,p);
  if (p->i!=2)
    abort();
}

int main ()
{
  A a={1};
  bar(&a);
  return 0;
}
