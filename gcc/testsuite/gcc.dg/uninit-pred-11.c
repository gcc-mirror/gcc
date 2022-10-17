/* { dg-do compile } */
/* { dg-options "-O -Wmaybe-uninitialized" } */

extern unsigned bar (void);
extern void quux (void);
int z;
unsigned foo (unsigned v, int y)
{
  unsigned u;
  if (v != 1)
    u = bar ();

  // Prevent the "dom" pass from changing the CFG layout based on the inference
  // 'if (v != 1) is false then (v != 2) is true'.  (Now it would have to
  // duplicate the loop in order to do so, which is deemed expensive.)
  for (int i = 0; i < 10; i++)
    quux ();

  // This variation from uninit-25.c causes compute_control_dep_chain
  // to run into a defect but simple_control_dep_chain saves us here
  if (y)
    z = 1;
  if (v != 1)
    return u;       /* { dg-bogus "may be used uninitialized" } */

  return 0;
}
