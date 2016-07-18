/* Make sure the compiler does not try to use a relative long
   instruction to load the string since it might not meet the
   alignment requirements of the instruction.  */

/* { dg-do compile } */
/* { dg-options "-march=z10 -O3 -mzarch" } */

extern void foo (char*);

void
bar ()
{
    unsigned char z[32];

    __builtin_memcpy (z, "\001\000\000\000", 4);
    foo (z);
}

/* { dg-final { scan-assembler-not "lrl" } } */
