/* { dg-do compile }
   { dg-options "-O -Wall" } */

void __flat *
convert_lds_addr (void __lds *x)
{ return x; }

/* { dg-final { scan-assembler "shared_base" } }  */
