/* PR rtl-optimization/127246 */
/* { dg-do compile } */
/* { dg-require-effective-target label_values } */
/* { dg-require-effective-target indirect_jumps } */
/* { dg-options "-O2 -fno-tree-slp-vectorize -fdump-rtl-jump2-details" } */

/* pass_duplicate_computed_gotos copies the dispatch back into every handler,
   so tail merging must not share the identical handler tails in front of
   it.  */

struct frame
{
  const unsigned short *instr_ptr;
  long *stackpointer;
};

#define OPS(X) \
  X (0) X (1) X (2) X (3) X (4) X (5) X (6) X (7) X (8) X (9) X (10) \
  X (11) X (12) X (13) X (14) X (15) X (16) X (17) X (18) X (19)

extern long op (long);

#define DISPATCH() goto *targets[*next_instr & 0xff]

#define HANDLER(N) \
  L_##N: \
    frame->instr_ptr = next_instr; \
    next_instr += 1; \
    frame->stackpointer = stack_pointer; \
    stack_pointer[-1] = op (N); \
    stack_pointer -= 1; \
    DISPATCH ();

#define ENTRY(N) &&L_##N,

void
interp (struct frame *frame)
{
  static const void *const targets[] = { OPS (ENTRY) };
  const unsigned short *next_instr = frame->instr_ptr;
  long *stack_pointer = frame->stackpointer;

  DISPATCH ();

  OPS (HANDLER)
}

/* { dg-final { scan-rtl-dump-not "Cross jumping" "jump2" } } */
