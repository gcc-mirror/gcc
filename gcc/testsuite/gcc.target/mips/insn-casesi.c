/* { dg-do run } */
/* { dg-options "-mips16 -mcode-readable=yes" } */

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
	.set	mips16
	.set	nomicromips
	.ent	frob
	.type	frob, @function
frob:
	.frame	$sp,0,$31		# vars= 0, regs= 0/0, args= 0, gp= 0
	.mask	0x00000000,0
	.fmask	0x00000000,0
	addiu	$2,$4,5
	sltu	$2,11
	bteqz	$L2
	sll	$3,$2,1
	la	$2,$L4
	addu	$3,$2,$3
	lh	$3,0($3)
	addu	$2,$2,$3
	j	$2
	.align	1
	.align	2
$L4:
	.half	$L3-$L4
	.half	$L2-$L4
	.half	$L9-$L4
	.half	$L2-$L4
	.half	$L2-$L4
	.half	$L8-$L4
	.half	$L2-$L4
	.half	$L2-$L4
	.half	$L7-$L4
	.half	$L2-$L4
	.half	$L8-$L4
$L8:
	.set	noreorder
	.set	nomacro
	jr	$31
	move	$2,$4
	.set	macro
	.set	reorder

$L9:
	li	$2,1
	.set	noreorder
	.set	nomacro
	jr	$31
	neg	$2,$2
	.set	macro
	.set	reorder

$L3:
	li	$2,2
	.set	noreorder
	.set	nomacro
	jr	$31
	neg	$2,$2
	.set	macro
	.set	reorder

$L7:
	.set	noreorder
	.set	nomacro
	jr	$31
	li	$2,1
	.set	macro
	.set	reorder

$L2:
	.insn
	.end	frob
	.size	frob, .-frob

  for `frob' and we want to make sure it links correctly owing to the
  `.insn' pseudo-op which needs to be there at `$L2' as there's no
  code following and the label is a MIPS16 branch target (even though
  the branch is never taken.  See also insn-tablejump.c.  */
