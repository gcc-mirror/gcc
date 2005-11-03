/* Test that SPE targets do not permit -m64.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile { target powerpc-*-*spe } } */
/* { dg-options "-m64" } */

/* { dg-error "-m64 not supported in this configuration" "SPE not 64-bit" { target *-*-* } 0 } */
