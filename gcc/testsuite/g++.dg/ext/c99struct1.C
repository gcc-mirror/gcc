// { dg-options "" }
// C99 anon struct variable with array accesses.

struct s { int a[1]; };

void
foo5 (void)
{
  int i = ((struct s) { { 0 } }).a[0];
}


