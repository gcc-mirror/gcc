/* Same state as pr123625-2.c (one variable coalesced into two other
   variables' spilled partitions) reached through asm goto control flow.  */

typedef long __attribute__((vector_size (16 * sizeof (long)))) v16di;

v16di g0, g1, g2, g3;

int
main (void)
{
  v16di b = g0;
  v16di a;
  asm goto ("" : : : : L0);
  a = b;
  goto L1;
L0:
  a = g1;
L1:
  g1 = a;
  b = g2;
  v16di c;
  asm goto ("" : : : : L2);
  c = b;
  goto L3;
L2:
  c = g3;
L3:
  g3 = c;
  if (g1[0] | g1[1] | g3[0] | g3[1])
    __builtin_abort ();
  return 0;
}
