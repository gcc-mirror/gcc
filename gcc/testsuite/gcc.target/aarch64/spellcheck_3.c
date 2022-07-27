/* { dg-do compile } */

__attribute__((target ("tune=cortex-a57-typo"))) void
foo ()
{
  /* { dg-message "valid arguments are: \[^\n\r]*; did you mean 'cortex-a57'?"  "" { target *-*-* } .-1 } */
  /* { dg-error "invalid name 'cortex-a57-typo' in 'target\\(\"tune=\"\\)' pragma or attribute"  "" { target *-*-* } .-2 } */
  /* { dg-error "pragma or attribute 'target\\(\"tune=cortex-a57-typo\"\\)' is not valid"  "" { target *-*-* } .-3 } */
}
