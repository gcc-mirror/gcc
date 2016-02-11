/* PR tree-optimization/69214 */

extern void bar (void);
extern int __setjmp (char *);

void
foo (char *p)
{
  int d = 0;
  bar ();
  if (__setjmp (p))
    return;
  long a = d;
  d = 8;
  if (!a)
    bar ();
}
