/* PR c/5656
   This testcase ICEd on IA-32 at -O3, due to tree inliner not converting
   parameter assignment when using K&R syntax.  */

void foo (c)
     char c;
{
  (void) &c;
}

int bar (void);

void baz (void)
{
  foo (bar ());
}
