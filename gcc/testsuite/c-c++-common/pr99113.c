/* { dg-do compile } */
/* { dg-options "-Wall -O2" } */

static int xyzzy __attribute__((__used__)) = 1; 

/* { dg-final { scan-assembler "xyzzy" } } */
/* { dg-final { scan-assembler-not "\.data.*,\"awR\"" { target R_flag_in_section } } } */
