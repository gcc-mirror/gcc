/* PR 10795.  */

/* ix86_expand_carry_flag_compare() in i386.c swapped the comparison
   operands without checking that the compare instruction, cmpl, would
   accept the swapped operands.  */

extern const char a[];

int
foo (const char *p)
{
  return (p > a) ? 0 : 2;
}
