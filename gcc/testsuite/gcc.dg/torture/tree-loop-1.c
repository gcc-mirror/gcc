/* PR tree-optimization/20640 */

/* After unrolling the loop, we'd turn some conditional branches into
   unconditional ones, but branch redirection would fail to compute
   the PHI args for the PHI nodes in the replacement edge
   destination, so they'd remain NULL causing crashes later on.  */

/* { dg-do compile } */

static int a = 0;
extern int foo (void);
extern int *bar (void) __attribute__ ((__const__));

void
test (int x)
{
  int b = 10;
  while (foo () == -1 && *bar () == 4 && b > 0)
    --b;
  a = x;
}
