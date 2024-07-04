/* { dg-options "-fno-move-loop-invariants -funroll-all-loops" } */
/* PR target/113221 */
/* This used to ICE after the `load/store pair fusion pass` was added
   due to the predicate aarch64_ldp_reg_operand allowing too much. */


void bar();
void foo(int* b) {
  for (;;)
    *b++ = (__SIZE_TYPE__)bar;
}

