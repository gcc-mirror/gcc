/* { dg-do compile } */
/* { dg-options "-O2 -mtune-ctrl=^prologue_using_move,^epilogue_using_move -fomit-frame-pointer -mnoreturn-no-callee-saved-registers" } */

#define ARRAY_SIZE 256

extern int array[ARRAY_SIZE][ARRAY_SIZE][ARRAY_SIZE];
extern int value (int, int, int)
#ifndef __x86_64__
__attribute__ ((regparm(3)))
#endif
;

void
_Noreturn
no_return_to_caller (void)
{
  unsigned i, j, k;
  for (i = ARRAY_SIZE; i > 0; --i)
    for (j = ARRAY_SIZE; j > 0; --j)
      for (k = ARRAY_SIZE; k > 0; --k)
	array[i - 1][j - 1][k - 1] = value (i, j, k);
  while (1);
}

/* { dg-final { scan-assembler-not "push\[^\n\r\]*(?:\[abcd\]x|\[sd\]i|sp|r\[0-9\]|\[xyz\]mm)" } } */
/* { dg-final { scan-assembler-not "pop\[^\n\r\]*(?:\[abcd\]x|\[sd\]i|sp|r\[0-9\]|\[xyz\]mm)" } } */
