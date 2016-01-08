/* PR tree-optimization/69167 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

int sigsetjmp (char *);
void foo ();
void bar (void (*) (int *));
extern char t[];

void
baz (int *x)
{
  int *a = x;
  foo ();
  x = 0;
  if (sigsetjmp (t))
    while (1)
      bar (a ? baz : 0);
  if (x)
    foo ();
}
