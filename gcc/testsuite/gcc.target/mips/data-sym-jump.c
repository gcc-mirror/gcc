/* { dg-do compile } */
/* { dg-options "-mips16 -mcode-readable=yes" } */
/* { dg-skip-if "MIPS16 `casesi' loses at -Os" { *-*-* } { "-Os"} { "" } } */

int
frob (int i)
{
  switch (i)
    {
    case -5:
      return -2;
    case -3:
      return -1;
    case 0:
      return 0;
    case 3:
      return 1;
    case 5:
      break;
    default:
      __builtin_unreachable ();
    }
  return i;
}

/* Expect assembly like:

	la	$2,$L4
						# Anything goes here.
	.type	__jump_frob_4, @object		# Symbol # must match label.
__jump_frob_4:					# The symbol must match.
$L4:						# The label must match.
	.half	$L3-$L4				# Or `.word'.  The subtrahend
	.half	$L2-$L4				# label must match thoughout
	.half	$L9-$L4				# (repeated 11 times).
	.half	$L2-$L4				# .
	.half	$L2-$L4				# .
	.half	$L8-$L4				# .
	.half	$L2-$L4				# .
	.half	$L2-$L4				# .
	.half	$L7-$L4				# .
	.half	$L2-$L4				# .
	.half	$L8-$L4				# .
	.type	__jend_frob_4, @function	# Symbol # must match label.
__jend_frob_4:					# The symbol must match.
	.insn

   that is `__jump_*'/`__jend_*' symbols inserted around a jump table.  */

/* { dg-final { scan-assembler "\tla\t\\\$\[0-9\]+,(.L(\[0-9\]+))\n.*\t\\.type\t(__jump_frob_\\2), @object\n\\3:\n\\1:\n(?:\t\\.(?:half|word)\t.L\[0-9\]+-\\1\n)\{11\}\t\\.type\t(__jend_frob_\\2), @function\n\\4:\n\t\\.insn\n" } } */
