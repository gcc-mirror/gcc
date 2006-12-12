/* { dg-do compile } */
/* { dg-options "-mfixed-range=1-x" } */
/* { dg-error "unknown register name" "" { target spu-*-* } 0 } */

int i;
