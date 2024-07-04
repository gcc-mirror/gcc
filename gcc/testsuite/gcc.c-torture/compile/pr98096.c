/* Test for correct naming of label operands in asm goto in case of presence of
   input/output operands. */
/* { dg-do compile { target asm_goto_with_outputs } } */
int i, j;
int f(void) {
  asm goto ("# %0 %2" : "+r" (i) ::: jmp);
  i += 2;
  asm goto ("# %0 %1 %l[jmp]" : "+r" (i), "+r" (j) ::: jmp);
 jmp: return i;
}
