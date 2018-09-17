/* Test the nvptx atomic instructions for __atomic_fetch_OP for
   SImode arguments.  */

/* { dg-do compile } */
/* { dg-options "-O2 -m32" } */

int
main()
{
  unsigned long a = ~0;
  unsigned b = 0xa;

  __atomic_fetch_add (&a, b, 0);
  __atomic_fetch_and (&a, b, 0);
  __atomic_fetch_or (&a, b, 0);
  __atomic_fetch_xor (&a, b, 0);
  
  return a;
}

/* { dg-final { scan-assembler "atom.add.u32" } } */
/* { dg-final { scan-assembler "atom.b32.and" } } */
/* { dg-final { scan-assembler "atom.b32.or" } } */
/* { dg-final { scan-assembler "atom.b32.xor" } } */
