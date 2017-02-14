/* { dg-do compile } */
/* { dg-require-effective-target arm_cortex_m } */
/* { dg-skip-if "-mthumb given" { *-*-* } { "-mthumb" } } */
/* { dg-options "-marm" } */
/* { dg-error "target CPU does not support ARM mode" "missing error with -marm on Thumb-only targets" { target *-*-* } 0 } */

/* Check that -marm gives an error when compiling for a Thumb-only target.  */

int foo;
