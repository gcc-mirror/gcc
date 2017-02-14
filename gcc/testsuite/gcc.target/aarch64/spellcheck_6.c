/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-mtune=*" } { "" } } */
/* { dg-options "-mtune=cortex-a72-typo" } */

void
foo ()
{
}

/* { dg-error "unknown value 'cortex-a72-typo' for -mtune"  "" { target *-*-* } 0 } */
/* { dg-message "valid arguments are: \[^\n\r]*; did you mean 'cortex-a72'?"  "" { target *-*-* } 0 } */
