/* { dg-do compile } */

__attribute__((target ("tune=cortex-a57-typo"))) void
foo ()
{
  /* { dg-message "valid arguments are: \[^\n\r]*; did you mean 'cortex-a57?"  "" { target *-*-* } .-1 } */
  /* { dg-error "unknown value 'cortex-a57-typo' for 'tune' target attribute"  "" { target *-*-* } .-2 } */
  /* { dg-error "target attribute 'tune=cortex-a57-typo' is invalid"  "" { target *-*-* } .-3 } */
}
