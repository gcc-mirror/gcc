/* PR c/90898 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

void *p;
int bar (void);
void baz (int *);

void
foo (void)
{
  p = __builtin_stack_save ();
  int a[(bar (), 2)];
  baz (a);
  __builtin_stack_restore (p);
}
