/* { dg-do compile } */
/* { dg-options "-Wall -O2" } */

static int xyzzy __attribute__((__used__)) = 1; 

void foo()
{
  int x __attribute__((__used__)); /* { dg-warning "attribute ignored|unused variable" } */
}

/* { dg-final { scan-assembler "xyzzy" } } */
/* { dg-final { scan-assembler-not "\.data.*,\"awR\"" { target R_flag_in_section } } } */
