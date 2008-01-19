/* The asm has 2 "r" in/out operands, 1 earlyclobber "r" output, 1 "r"
   input and 2 fixed "r" clobbers (eax and edx), so there are a total of
   6 registers that must not conflict.  Add to that the PIC register,
   the frame pointer, and the stack pointer, and we've run out of
   registers on 32-bit targets.  */
/* { dg-do compile { target { { ! ilp32 } || nonpic } } } */
/* { dg-options "-O" } */

typedef unsigned long bngdigit;
typedef bngdigit *bng;
typedef unsigned int bngcarry;
typedef unsigned long bngsize;

bngdigit
bng_ia32_mult_sub_digit (bng a, bngsize alen, bng b, bngsize blen, bngdigit d)
{
  bngdigit out, tmp;
  bngcarry carry;
  bngdigit a11;

  alen -= blen;
  out = 0;
  asm (""
       : "+r" (a), "+r" (b), "+mr" (blen), "+mr" (out), "=&r" (tmp)
       : "mr" (d)
       : "eax", "edx");
  if (alen == 0)
    {
      a11 = out;
      goto t;
    }

  a11 = 1;
 t:
  return a11;
}
