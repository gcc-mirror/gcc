/* PR debug/43299 */
/* { dg-do assemble } */
/* { dg-options "-g -O2" } */

extern void *emit_insn (void *);

__attribute__((noinline))
void *gen_load_locked_si (void *x, void *y)
{
  return x;
}

__attribute__((noinline))
void *gen_load_locked_di (void *x, void *y)
{
  return x;
}

void
emit_load_locked (int mode, void *reg, void *mem)
{
  void * (*fn) (void *, void *) = ((void *)0);
  if (mode == 9)
    fn = gen_load_locked_si;
  else if (mode == 10)
    fn = gen_load_locked_di;
  emit_insn (fn (reg, mem));
}
