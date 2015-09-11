/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-ccp1-details" } */

extern "C" void abort (void);
inline void *operator new (__SIZE_TYPE__, void *__p) throw () { return __p; }

int foo(void)
{
  float f = 0;
  int *i = new (&f) int (1);
  return *(int *)&f;
}

/* { dg-final { scan-tree-dump "Folded into: if \\\(1 != 0\\\)" "ccp1" } } */
