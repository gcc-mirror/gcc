/* { dg-do compile { target gnu_retain } } */
/* { dg-options "-Wall -O2" } */
/* Alpha places small objects in gp-relative sdata/sbss, which carry an
   extra "s" section flag the scans do not match. */
/* { dg-additional-options "-G0" { target alpha*-*-* } } */

static int xyzzy __attribute__((__used__, __retain__)) = 1; 

void foo()
{
  int x __attribute__((__retain__)); /* { dg-warning "attribute ignored|unused variable" } */
}

/* { dg-final { scan-assembler "xyzzy" } } */
/* { dg-final { scan-assembler "\.data.*,\"awR\"" { target gnu_retain } } } */
