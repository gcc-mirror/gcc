/* PR optimization/7675 */
/* Contributed by Volker Reichelt */

/* Verify that we don't put automatic variables
   in registers too early.  */

extern int dummy (int *);

void foo(int i)
{
  int j=i;

  void bar() { int x=j, y=i; }

  dummy(&i);
}
