/* { dg-do compile } */
/* { dg-options "-mmicromips" } */

void
unreachable (int i)
{
  asm volatile goto ("b\t.\n\tbeqz\t%0,%l1" : : "r" (i) : : punt);
punt:
  __builtin_unreachable ();
}

/* Expect assembly like:

	beqz	$4,$L2
				# Anything goes here.
$L2:				# The label must match.
	.insn
$L3 = .				# It's there, but we don't care.
	.end	unreachable

   that is .insn to be inserted if a code label is at function's end.  */

/* { dg-final { scan-assembler "\tbeqz\t\\\$\[0-9\]+,(.L\[0-9\]+)\n.*\n\\1:\n\t\\.insn\n(?:.L\[0-9\]+ = \\.\n)?\t\\.end\tunreachable\n" } } */
