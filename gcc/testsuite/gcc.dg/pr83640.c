/* PR tree-optimization/83640 - ice in generic_overlap, at
   gimple-ssa-warn-restrict.c:814
   { dg-do compile }
   { dg-options "-O2 -Wall" }  */

char *foo (void);

void
bar (char *b, char *c)
{
  b = c;
  c = foo ();
  __builtin_strcat (c, "*/");
  __builtin_strcat (c, b);
}
