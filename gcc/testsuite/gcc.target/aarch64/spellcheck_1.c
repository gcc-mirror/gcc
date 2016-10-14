/* { dg-do compile } */

__attribute__((target ("arch=armv8-a-typo"))) void
foo ()
{
}
/* { dg-message "valid arguments are: \[^\n\r]*; did you mean 'armv8-a'?"  "" { target *-*-* } 5 } */
/* { dg-error "unknown value 'armv8-a-typo' for 'arch' target attribute"  "" { target *-*-* } 5 } */
/* { dg-error "target attribute 'arch=armv8-a-typo' is invalid"  "" { target *-*-* } 5 } */
