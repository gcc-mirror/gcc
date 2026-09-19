/* { dg-do compile } */
/* { dg-options "-O2 -march=rv32i_zilsd -mabi=ilp32" } */
/* { dg-additional-options "-mzilsd-strict-align" } */

/* On RV32 an int64_t argument passed after seven pointer arguments is
   split between registers and the stack: the low word lands in the last
   available argument register (a7) and the high word on the incoming
   stack.  The middle-end reconstructs the value starting at a
   4-byte-aligned stack slot (offset 12), so under -mzilsd-strict-align
   the backend must not access it with a single misaligned zilsd "ld"; it
   must split the access into two 32-bit loads instead.  */

long long
f7 (long long *a, long long *b, long long *c, long long *d,
    long long *e, long long *f, long long *g, long long n)
{
  return n;
}

/* The misaligned access must use two 32-bit "lw", never a single zilsd
   "ld" from the 4-byte-aligned stack offset 12.  */
/* { dg-final { scan-assembler-not {ld\t[a-z0-9]+,12\(sp\)} } } */
