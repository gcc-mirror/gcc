/* { dg-do compile } */

__attribute__((target ("cpu=cortex-a57-typo"))) void
foo ()
{
}
/* { dg-message "valid arguments are: \[^\n\r]*; did you mean 'cortex-a57?"  "" { target *-*-* } 5 } */
/* { dg-error "unknown value 'cortex-a57-typo' for 'cpu' target attribute"  "" { target *-*-* } 5 } */
/* { dg-error "target attribute 'cpu=cortex-a57-typo' is invalid"  "" { target *-*-* } 5 } */
