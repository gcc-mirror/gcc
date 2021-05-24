/* { dg-do compile } */ 
/* { dg-options "-O2 -fdump-tree-sink-stats" } */
extern int foo (int *, int *);
extern int foo2 (int);
int
main (int argc)
{
  int a, b, c;
  b = argc + 1;
  c = argc + 2;
  a = b + c;
  if (argc)
    {
      foo (&b, &c);
      a = b + c;
    }
  foo2 (a);
}
/* We should sink the first a = b + c calculation into the else branch  */
/* { dg-final { scan-tree-dump-times "Sunk statements: 1" 1 "sink1" } } */
