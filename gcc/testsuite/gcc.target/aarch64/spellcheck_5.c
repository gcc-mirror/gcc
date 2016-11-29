/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-mcpu=*" } { "" } } */
/* { dg-options "-mcpu=cortex-a17" } */

void
foo ()
{
}

/* { dg-error "unknown value 'cortex-a17' for -mcpu"  "" { target *-*-* } 0 } */
/* { dg-message "valid arguments are: \[^\n\r]*; did you mean 'cortex-a57'?"  "" { target *-*-* } 0 } */
