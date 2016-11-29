/* { dg-do compile } */
/* { dg-options "-mips16 -mcode-readable=yes" } */

void
unreachable (void)
{
  asm volatile goto ("b\t.\n\tbeqz\t%0,%l1" : : "r" (0x12345678) : : punt);
punt:
  __builtin_unreachable ();
}

/* Expect assembly like:

	lw	$2,$L5
				# Anything goes here.
	beqz	$2,$L2		# The register must match.
				# Anything goes here.
$L2:				# The label must match.
	.insn
$L3 = .				# It's there, but we don't care.
	.type	__pool_unreachable_5, @object
__pool_unreachable_5:
	.align	2
$L5:				# The label must match.
	.word	305419896

   that is .insn to be inserted if a code label is at a constant pool.  */

/* { dg-final { scan-assembler "\tlw\t(\\\$\[0-9\]+),(.L\[0-9\]+)\n.*\tbeqz\t\\1,(.L\[0-9\]+)\n.*\n\\3:\n\t\\.insn\n(?:.L\[0-9\]+ = \\.\n)?\t\\.type\t__pool_unreachable_\[0-9\]+, @object\n__pool_unreachable_\[0-9\]+:\n\t\\.align\t2\n\\2:\n\t\\.word\t305419896\n" } } */
