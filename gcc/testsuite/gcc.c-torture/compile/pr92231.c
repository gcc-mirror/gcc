/* PR middle-end/92231 */

extern int bar (void);

int
foo (void)
{
  return (&bar + 4096) ();
}
