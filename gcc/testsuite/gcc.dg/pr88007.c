/* PR other/88007 */
/* { dg-do compile } */
/* { dg-options "--param ggc-min-expand=3 --param ggc-min-heapsize=1024" } */
/* { dg-skip-if "no code alignment > 2" { "pdp11-*-*" } } */

void bar (void);

__attribute__((optimize ("align-loops=16", "align-jumps=16",
			 "align-labels=16", "align-functions=16")))
void
foo (void)
{
  for (int i = 0; i < 1024; ++i)
    bar ();
}
