/* AArch64 wrong code at -O2.  Store motion creates several SSA versions of an
   oversized vector temporary (V16DI, 128 bytes, no register mode).  The
   partition holding the loop-carried versions has an anonymous representative,
   so out-of-SSA left it and the partition of the copy taken for the use after
   the loop sharing one MEM_EXPR.  The load/store pair-fusion pass then treated
   two stores 144 bytes apart as adjacent, fused them, and left the tail of one
   slot uninitialised.  Self-checking: aborts if the result is wrong.  */

typedef long __attribute__((vector_size (16 * sizeof (long)))) v16di;
typedef int __attribute__((vector_size (16 * sizeof (int)))) v16si;

long g2, g12;
v16di g18;
v16si g3;
void *g27;

/* The wrong value is read from an uninitialised stack slot, so make sure the
   stack the callee reuses does not happen to be zero.  */
__attribute__((noipa)) static void
dirty_stack (void)
{
  volatile char buf[1024];
  for (unsigned i = 0; i < sizeof (buf); i++)
    buf[i] = 0xa5;
}

void
f31 (void)
{
lbl_br1:
  g18 = ~g18;
  g3 = ~g3;
  if (g2)
    goto lbl_br1;
lbl_b5:
  switch (g12)
    case 4:
    case 0:
      goto lbl_sw8;
  __builtin_abort ();
lbl_sw8:
  if (g27)
    goto lbl_b5;
  g18 = ~g18;
}

int
main (void)
{
  dirty_stack ();
  f31 ();
  for (int i = 0; i < 16; i++)
    if (g18[i] != 0)
      __builtin_abort ();
  return 0;
}
