/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-march=*" } { "" } } */
/* { dg-skip-if "" { *-*-* } { "-mcpu=*" } { "" } } */
/* { dg-options "-mcpu=armv8.2-a+dotprod" } */

void
foo ()
{
}

/* { dg-error "unknown value .armv8.2-a\\+dotprod. for .-mcpu."  "" { target *-*-* } 0 } */
/* { dg-message "valid arguments are: \[^\n\r]*"  "" { target *-*-* } 0 } */
/* { dg-message "did you mean .-march=armv8.2-a\\+dotprod.?"  "" { target *-*-* } 0 } */
