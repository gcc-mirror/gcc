/* Verify that a diagnostic is issued without crashing due to
   --enable-checking catching a bug in the C front end.  */
/* { dg-do compile } */
x()
{
  foo (i);
  /* { dg-error "undeclared" "undeclared-variable message" { target native } { 6 } } */
  /* { dg-error "function it appears in" "reminder message" { target native } { 6 } } */ 
}
