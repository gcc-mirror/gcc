/* Test that large 64-bit constants are loaded with llihf + oilf when lgrl is
   available.  */

/* { dg-do compile } */
/* { dg-options "-O3 -march=z10" } */

unsigned long long
magic (void)
{
  return 0x3f08c5392f756cdULL;
}

/* { dg-final { scan-assembler-times {\n\tllihf\t} 1 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\n\toilf\t} 1 { target lp64 } } } */
