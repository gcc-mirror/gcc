/* PR c/11428.  */

/* fold_single_bit_test() failed to return a tree of the type that the
   outer expression was looking for.  Specifically, it returned a tree
   whose type corresponded to QImode for !p->m, but the desired result
   type was int, which corresponded to SImode.  emit_move_insn() later
   tried to copy a reg:QI to reg:SI, causing an ICE.  */

struct s {
  int m : 1;
};

int
foo (struct s *p)
{
  return !p->m;
}
