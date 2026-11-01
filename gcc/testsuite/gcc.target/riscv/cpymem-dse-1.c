/* RTL DSE can only eliminate the copy of a word that a later store
   overwrites if the copy's loads are not interleaved with its stores.
   riscv_block_move_straight must therefore keep the trailing whole-word
   chunk in the group instead of deferring it to move_by_pieces, which
   emits its accesses after the group's stores.  */

/* { dg-do compile } */
/* { dg-options "-march=rv32gc -mabi=ilp32d" { target { rv32 } } } */
/* { dg-options "-march=rv64gc -mabi=lp64d" { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } {"-O0" "-Os" "-Og" "-Oz" "-flto" } } */

struct Record
{
  struct Record *Ptr_Comp;
  long Discr;
  long Enum_Comp;
  long Int_Comp;
  char Str_Comp[32];
};

extern struct Record Glob;

void
Proc (struct Record *p)
{
  struct Record *n = p->Ptr_Comp;
  *n = Glob;
  n->Int_Comp = 5;
}

/* The word holding Int_Comp (offset 12 on rv32, 24 on rv64) is
   immediately overwritten, so the copy of that word (a load from that
   offset in Glob feeding a store to the same offset in *n) must have
   been removed.  The store of the constant 5 remains.  */
/* { dg-final { scan-assembler-not "lw\t\[at\]\[0-9\]+,12\\(" { target rv32 } } } */
/* { dg-final { scan-assembler-not "ld\t\[at\]\[0-9\]+,24\\(" { target rv64 } } } */
