/* { dg-options "-Os -march=rv32imc -mabi=ilp32" } */

/* These stores cannot be compressed because x0 is not a compressed reg.
   Therefore the shorten_memrefs pass should not attempt to rewrite them into a
   compressible format.  */

void
store1z (int *array)
{
  array[200] = 0;
  array[201] = 0;
  array[202] = 0;
  array[203] = 0;
}

void
store2z (long long *array)
{
  array[200] = 0;
  array[201] = 0;
  array[202] = 0;
  array[203] = 0;
}

/* { dg-final { scan-assembler-not "store1z:\n\taddi" } } */
/* { dg-final { scan-assembler-not "store2z:\n\taddi" } } */
