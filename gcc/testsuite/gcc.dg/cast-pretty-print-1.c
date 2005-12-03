/* Test pretty-printing of casts.  Should not depend on whether
   NOP_EXPR or CONVERT_EXPR is used.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "" } */
int i;
void
f (void)
{
  ((unsigned int)i)(); /* { dg-error "error: called object '\\(unsigned int\\)i' is not a function" } */
  ((char)i)(); /* { dg-error "error: called object '\\(char\\)i' is not a function" } */
}
