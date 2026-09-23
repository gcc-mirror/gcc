/* The same out-of-SSA defect as pr126405.c, with a narrower companion vector.
   That changes the register pressure around the copy of the oversized vector
   and so the pair the fusion pass picks, but the cause is the same: two stack
   slots of one variable sharing a MEM_EXPR.  Self-checking: aborts if the
   result is wrong.  */

typedef long __attribute__((vector_size (16 * sizeof (long)))) v16di;
typedef int __attribute__((vector_size (8 * sizeof (int)))) v8si;

long g2, g12;
v16di g18;
v8si g3;
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
