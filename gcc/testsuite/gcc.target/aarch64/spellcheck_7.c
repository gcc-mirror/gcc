/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-march=*" } { "" } } */
/* { dg-skip-if "" { *-*-* } { "-mcpu=*" } { "" } } */
/* { dg-options "-march=armv8-a+typo" } */

void
foo ()
{
}

/* { dg-error "invalid feature modifier .typo. in .-march=armv8-a\\+typo."  "" { target *-*-* } 0 } */
/* { dg-message "valid arguments are: \[^\n\r]*"  "" { target *-*-* } 0 } */
