/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -fomit-frame-pointer" } */

void foo (long long ixi)
{
  if (ixi != 14348907)
    __builtin_abort ();
}

/* { dg-final { scan-assembler-times "mov" 1 { target nonpic } } } */
/* get_pc_thunk adds an extra mov insn.  */
/* Choosing a non-bx get_pc_thunk requires another mov before the abort call.
   So we require a match of either that mov or the get_pc_thunk.bx call, in
   addition to the other 2 movs.  (Hopefully there won't be more calls for a
   false positive.)  */
/* { dg-final { scan-assembler-times "mov|call\[^\n\r]*get_pc_thunk\.bx" 3 { target { ! nonpic } } } } */
