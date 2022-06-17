/* PR target/99753 */

/* { dg-do compile } */
/* { dg-options "-march=amd" } */
/* { dg-error "bad value 'amd' for '-march=' switch"  "" { target *-*-* } 0 } */
