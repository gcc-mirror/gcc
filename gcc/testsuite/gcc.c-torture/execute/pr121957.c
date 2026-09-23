/* AArch64 wrong code at -O2/-O3.  An oversized vector
   (V16DI, 128 bytes, with no register mode) is expanded into two distinct
   stack slots that share the same MEM_EXPR.  The load/store pair-fusion pass
   then discovers "adjacent" store pairs across the two slots via the shared
   MEM_EXPR and fuses them, redirecting a store to the wrong slot and leaving
   part of a slot uninitialised.  Self-checking: aborts if the result is
   wrong.  */

typedef long __attribute__((vector_size (16 * sizeof (long)))) v16di;

int
main (void)
{
  v16di v = {};
  asm goto ("" : : : : L1);
L2:
  asm goto ("" : : : : L1);
L0:
  asm goto ("" : : : : L2);
  v = (v16di){ -1 };
  asm goto ("" : : : : L0);
L1:
  asm goto ("" : : : : L0);
  if (v[3])
    __builtin_abort ();
  return 0;
}
