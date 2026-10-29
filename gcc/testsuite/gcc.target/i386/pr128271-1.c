/* { dg-do compile } */
/* { dg-options "-O2 -mshstk -masm=att" } */

#include <immintrin.h>

void on_zero (void);

void
foo (long *counter, void *tok)
{
  long v = *counter - 1;
  *counter = v;
  _clrssbsy (tok);
  if (v == 0)
    on_zero ();
}

/* clrssbsy clobbers flags, so the comparison must not be eliminated.  */
/* { dg-final { scan-assembler "\[ \t\]test\[lq\]" } } */
