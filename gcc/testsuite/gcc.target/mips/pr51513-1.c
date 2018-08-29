/* { dg-do compile } */
/* { dg-options "-mips16 -mcode-readable=yes" } */

/* PR tree-optimization/51513 verification variant for MIPS16, #1.  */

int __attribute__ ((weak))
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

/* Without the fix for PR tree-optimization/51513 truncated code
   would be emitted for `frob', like:

	.text
	.align	2
	.weak	frob
	.set	mips16
	.set	nomicromips
	.ent	frob
	.type	frob, @function
frob:
	.frame	$sp,0,$31		# vars= 0, regs= 0/0, args= 0, gp= 0
	.mask	0x00000000,0
	.fmask	0x00000000,0
	addiu	$2,$4,5
	.end	frob
	.size	frob, .-frob

  meaning `frob' will have no chance to return, let alone produce
  the result expected.  */

/* { dg-final { scan-assembler "\tjrc?\t\\\$31\n" } } */
