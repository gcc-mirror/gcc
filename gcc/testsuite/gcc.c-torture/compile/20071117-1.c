/* PR middle-end/34134 */
/* { dg-require-effective-target alloca } */

extern void bar (void *, int);

int foo (int i)
{
  char *p = __builtin_stack_save ();
  void *q = __builtin_alloca (i);
  bar (q, i);
  __builtin_stack_restore (p);
  bar ("", 0);
  return 6;
}
