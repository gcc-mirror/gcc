/* Bug 8927: undeclared identifiers should give an error on the line
   of that identifier, not on the line of the next token.  */
/* { dg-do compile } */
/* { dg-options "" } */

void
foo(void)
{
  bar /* { dg-error "undeclared|for each function" } */


  ;
}
