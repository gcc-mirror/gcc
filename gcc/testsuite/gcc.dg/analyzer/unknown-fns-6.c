#include "analyzer-decls.h"

extern void unknown_side_effects (void);

struct s { int a; int b; };
struct s g;

void
test_global_struct (void)
{
  int implicit_g_b = g.b;
  __analyzer_eval (g.b == implicit_g_b); /* { dg-warning "TRUE" } */

  unknown_side_effects ();
  __analyzer_eval (g.b == implicit_g_b); /* { dg-warning "UNKNOWN" } */

  /* Write to g.a shouldn't affect g.b  */
  g.a = 1;
  __analyzer_eval (g.b == implicit_g_b); /* { dg-warning "UNKNOWN" } */
}
