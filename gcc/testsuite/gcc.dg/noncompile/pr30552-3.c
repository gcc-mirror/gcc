/* PR c/30552 */

/* Related example where statement expression used as old-style formal array 
 * argument size in an invalid nested function declaration should generate 
 * user error, not internal compiler error. */

/* { dg-do compile } */
/* { dg-options "" } */

int main()
{
  int g()
    int a[( {int b} )]; /* { dg-error "braced-group within expression allowed only inside a function|declaration for parameter" } */
  return 0; /* { dg-error "expected declaration specifiers before" } */
} /* { dg-error "expected declaration specifiers before|end of input|expected declaration or statement at end of input" } */
