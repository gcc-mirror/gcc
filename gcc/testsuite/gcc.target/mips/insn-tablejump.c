/* { dg-do run } */
/* { dg-options "-mmicromips" } */

int __attribute__ ((noinline))
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

int
main (int argc, char **argv)
{
  asm ("" : "+r" (argc));
  argc = frob ((argc & 10) - 5);
  asm ("" : "+r" (argc));
  return !argc;
}

/* This will result in assembly like:

	.text
	.align	2
	.globl	frob
	.set	nomips16
	.set	micromips
	.ent	frob
	.type	frob, @function
frob:
	.frame	$sp,0,$31		# vars= 0, regs= 0/0, args= 0, gp= 0
	.mask	0x00000000,0
	.fmask	0x00000000,0
	.set	noreorder
	.set	nomacro
	addiu	$3,$4,5
	sltu	$2,$3,11
	beqzc	$2,$L2
	lui	$2,%hi($L4)
	addiu	$2,$2,%lo($L4)
	lwxs	$3,$3($2)
	jrc	$3
	.rdata
	.align	2
	.align	2
$L4:
	.word	$L3
	.word	$L2
	.word	$L9
	.word	$L2
	.word	$L2
	.word	$L8
	.word	$L2
	.word	$L2
	.word	$L7
	.word	$L2
	.word	$L8
	.text
$L8:
	jr	$31
	move	$2,$4

$L9:
	jr	$31
	li	$2,-1			# 0xffffffffffffffff

$L3:
	jr	$31
	li	$2,-2			# 0xfffffffffffffffe

$L7:
	jr	$31
	li	$2,1			# 0x1

$L2:
	.insn
	.set	macro
	.set	reorder
	.end	frob
	.size	frob, .-frob

  for `frob' and we want to make sure it links correctly owing to the
  `.insn' pseudo-op which needs to be there at `$L2' as there's no
  code following and the label is a microMIPS branch target (even though
  the branch is never taken.  See also insn-casesi.c.  */
