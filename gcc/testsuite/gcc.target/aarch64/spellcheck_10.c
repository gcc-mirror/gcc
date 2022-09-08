/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-march=*" } { "" } } */
/* { dg-skip-if "" { *-*-* } { "-mcpu=*" } { "" } } */
/* { dg-options "-march=neoverse-n1" } */

void
foo ()
{
}

/* { dg-error "unknown value .neoverse-n1. for .-march."  "" { target *-*-* } 0 } */
/* { dg-message "valid arguments are: \[^\n\r]*"  "" { target *-*-* } 0 } */
/* { dg-message "did you mean .-mcpu=neoverse-n1.?"  "" { target *-*-* } 0 } */
