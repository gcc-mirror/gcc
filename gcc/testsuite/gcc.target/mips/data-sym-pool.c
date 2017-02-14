/* { dg-do compile } */
/* { dg-options "-mips16 -mcode-readable=yes" } */

int
frob (void)
{
  return 0x12345678;
}

/* Expect assembly like:

	lw	$2,$L3
						# Anything goes here.
	.type	__pool_frob_3, @object		# Symbol # must match label.
__pool_frob_3:					# The symbol must match.
	.align	2
$L3:						# The label must match.
	.word	305419896
	.type	__pend_frob_3, @function	# Symbol # must match label.
__pend_frob_3:					# The symbol must match.
	.insn

   that is `__pool_*'/`__pend_*' symbols inserted around a constant pool.  */

/* { dg-final { scan-assembler "\tlw\t\\\$\[0-9\]+,(.L(\[0-9\]+))\n.*\t\\.type\t(__pool_frob_\\2), @object\n\\3:\n\t\\.align\t2\n\\1:\n\t\\.word\t305419896\n\t\\.type\t(__pend_frob_\\2), @function\n\\4:\n\t\\.insn\n" } } */
