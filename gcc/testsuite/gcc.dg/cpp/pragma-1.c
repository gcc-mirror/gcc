/* Verify that preprocessor does not insert redundant newlines
   after #pragma */
/* { dg-do compile } */
int
main ()
{
#pragma unknown
  {
    error;
  /* { dg-error "undeclared" "undeclared-variable message" { target *-*-* } { 9 } } */
  /* { dg-error "function it appears in" "reminder message" { target *-*-* } { 9 } } */ 
  }
}
