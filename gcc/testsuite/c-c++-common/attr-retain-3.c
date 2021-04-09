/* { dg-do compile { target R_flag_in_section } } */
/* { dg-options "-Wall -O2 -fcommon" } */

static int xyzzy __attribute__((__used__, __retain__)); 

/* { dg-final { scan-assembler "xyzzy" } } */
/* { dg-final { scan-assembler ",\"awR\"" { target R_flag_in_section } } } */
