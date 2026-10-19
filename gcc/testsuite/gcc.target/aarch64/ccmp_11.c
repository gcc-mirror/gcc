/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-reassoc" } */
/* PR middle-end/109832 */

/* Rejected all-boolean subchains are cached during expansion.  */

extern _Bool t (void);

#define B t ()

_Bool
bool_call_chain (void)
{
  return B | B | B | B | B | B | B | B
	 | B | B | B | B | B | B | B | B
	 | B | B | B | B | B | B | B | B
	 | B | B | B | B | B | B | B | B;
}

/* { dg-final { scan-assembler-not {\tccmp\t} } } */
