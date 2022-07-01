/* PR tree-optimization/92879 - incorrect warning of __builtin_memset
   offset is out of the bounds on zero-size allocation and initialization
   { dg-do compile }
   { dg-options "-O2 -Wall -fdump-tree-optimized" } */

inline void* operator new (__SIZE_TYPE__, void * v)
{
  return v;
}

struct S
{
  int* p;
  int m;

  S (int i)
  {
    m = i;
    p = (int*) new unsigned char [sizeof (int) * m];

    for (int i = 0; i < m; i++)
      new (p + i) int (); /* { dg-bogus "bounds" "pr102690" { xfail *-*-* } } */
  }
};

S a (0);

/* The loop cannot be eliminated since the global 'new' can change 'm'.  */
/* { dg-final { scan-tree-dump-not "goto" "optimized" { xfail *-*-* } } } */
