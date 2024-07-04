/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -fomit-frame-pointer" } */

void foo (long long ixi)
{
  if (ixi != 14348907)
    __builtin_abort ();
}

/* { dg-final { scan-assembler-times "mov" 1 { target nonpic } } } */
/* get_pc_thunk adds an extra mov insn.  */
/* { dg-final { scan-assembler-times "mov" 2 { target { ! nonpic } } } } */
