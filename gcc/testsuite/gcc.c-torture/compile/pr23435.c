/* PR target/23435.

   On m68k-none-elf, this used to cause an unrecognized insn because
   zero_extendsidi2 accepted operands that are both memory even though
   such a pattern did not exist.  */

void
foo (unsigned long *a, unsigned long long *p)
{
  *p = *a;
}
