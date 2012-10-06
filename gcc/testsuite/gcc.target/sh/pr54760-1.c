/* Check that the __builtin_thread_pointer and __builtin_set_thread_pointer
   built-in functions result in gbr store / load instructions.  */
/* { dg-do compile { target "sh*-*-*" } } */
/* { dg-options "-O1" } */
/* { dg-skip-if "" { "sh*-*-*" } { "-m5*"} { "" } }  */
/* { dg-final { scan-assembler-times "ldc" 1 } } */
/* { dg-final { scan-assembler-times "stc" 1 } } */
/* { dg-final { scan-assembler-times "gbr" 2 } } */

void*
test00 (void)
{
  return __builtin_thread_pointer ();
}

void
test01 (void* p)
{
  __builtin_set_thread_pointer (p);
}
