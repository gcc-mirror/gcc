/* { dg-do compile } */
/* { dg-options "-Wall -O2 -fcommon" } */

int xyzzy __attribute__((__used__)); 

/* { dg-final { scan-assembler "xyzzy" } } */
/* { dg-final { scan-assembler-not ",\"awR\"" { target R_flag_in_section } } } */
