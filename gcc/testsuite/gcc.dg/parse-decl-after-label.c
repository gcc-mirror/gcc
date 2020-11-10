/* PR 29062 
{ dg-do compile }
{ dg-options "-std=c17 -pedantic-errors -fsyntax-only" }
*/

int f(int x)
{
  if (x > 1) 
    {
      goto finish;
    }
  return x;
  
 finish:
  int ret = 1; /* { dg-error "a label can only be part of a statement and a declaration is not a statement" } */
  return ret;
}
