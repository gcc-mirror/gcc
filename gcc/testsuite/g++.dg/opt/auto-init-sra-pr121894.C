/* Verify that SRA total scalarization will not be confused by padding
   and also not confused by auto initialization.  */
/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-esra -ftrivial-auto-var-init=pattern" } */

struct S { int a, b, c, d; };
void bar (int, int, int, int);

void
foo ()
{
  S s;
  s.a = 1;
  s.c = 2;
  s.d = 3;
  s.a++;
  s.c++;
  s.d++;
  bar (s.a, s.b, s.c, s.d);
}

/* { dg-final { scan-tree-dump-times "DEFERRED_INIT" 4 "esra" } } */
