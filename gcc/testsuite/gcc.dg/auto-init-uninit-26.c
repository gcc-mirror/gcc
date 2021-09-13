/* { dg-do compile } */
/* { dg-options "-O -Wmaybe-uninitialized -ftrivial-auto-var-init=zero" } */

extern unsigned bar (void);
extern void quux (void);

unsigned foo (unsigned v)
{
  unsigned u;
  if (v != 100)
    u = bar ();

  // Prevent the "dom" pass from changing the CFG layout based on the inference
  // 'if (v != 100) is false then (v < 105) is true'.  (Now it would have to
  // duplicate the loop in order to do so, which is deemed expensive.)
  for (int i = 0; i < 10; i++)
    quux ();

  if (v < 105) /* v == 100 falls into this range.  */
    return u;       /* { dg-warning "may be used uninitialized" }  */

  return 0;
}
