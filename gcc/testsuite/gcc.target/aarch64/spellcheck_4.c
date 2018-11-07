/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-march=*" } { "" } } */
/* { dg-options "-march=armv8-a-typo" } */

void
foo ()
{
}

/* { dg-error "unknown value 'armv8-a-typo' for -march"  "" { target *-*-* } 0 } */
/* { dg-message "valid arguments are: \[^\n\r]*(; did you mean 'armv*'?)?"  "" { target *-*-* } 0 } */
