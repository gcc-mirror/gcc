/* Check that the 'extu.b' instruction is generated for short jump tables.  */
/* { dg-do compile { target "sh*-*-*" } } */
/* { dg-options "-Os" } */
/* { dg-skip-if "" { "sh*-*-*" } { "-m5*"} { "" } }  */
/* { dg-final { scan-assembler "extu.b" } } */

int
test (int arg)
{
  int rc;
  switch (arg)
    {
    case 0:
      asm ("nop\n\tnop\n\tnop\n\tnop\n\tnop\n\t"
	   "nop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\t"
	   "mov r4,%0"
	   : "=r" (rc)
	   : "r" (arg));
      break;
    case 1:
      asm ("nop\n\tnop\n\tnop\n\tnop\n\tnop\n\t"
	   "nop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\t"
	   "mov r5,%0"
	   : "=r" (rc)
	   : "r" (arg));
      break;
    case 2:
      asm ("nop\n\tnop\n\tnop\n\tnop\n\tnop\n\t"
	   "nop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\t"
	   "mov r6,%0"
	   : "=r" (rc)
	   : "r" (arg));
      break;
    case 3:
      asm ("nop\n\tnop\n\tnop\n\tnop\n\tnop\n\t"
	   "nop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\t"
	   "mov r7,%0"
	   : "=r" (rc)
	   : "r" (arg));
      break;
    case 4:
      asm ("nop\n\tnop\n\tnop\n\tnop\n\tnop\n\t"
	   "nop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\t"
	   "mov r8,%0"
	   : "=r" (rc)
	   : "r" (arg));
      break;
    }
  return rc;
}
