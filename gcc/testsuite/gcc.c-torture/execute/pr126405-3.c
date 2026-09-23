/* A parameter that stays live across a redefinition of itself needs two
   partitions, and an oversized vector parameter (V16DI, 128 bytes, no register
   mode) puts both of them in memory.  Every name of both partitions has the
   PARM_DECL as its base, so without a split the incoming argument slot and the
   local slot both claim to be the parameter, and the load/store pair-fusion
   pass fuses accesses across them.  Self-checking: aborts if the result is
   wrong.  */

typedef long __attribute__((vector_size (16 * sizeof (long)))) v16di;

v16di g0, g1;

/* The wrong value is read from an uninitialised stack slot, so make sure the
   stack the callee reuses does not happen to be zero.  */
__attribute__((noipa)) static void
dirty_stack (void)
{
  volatile char buf[1024];
  for (unsigned i = 0; i < sizeof (buf); i++)
    buf[i] = 0xa5;
}

__attribute__((noipa)) static v16di
f (v16di p)
{
  v16di old = p;
  p = g0;
  g1 = p;
  return old + p;
}

int
main (void)
{
  v16di a, r;

  for (int i = 0; i < 16; i++)
    {
      a[i] = i + 1;
      g0[i] = 100;
    }
  dirty_stack ();
  r = f (a);
  for (int i = 0; i < 16; i++)
    if (r[i] != i + 101 || g1[i] != 100)
      __builtin_abort ();
  return 0;
}
