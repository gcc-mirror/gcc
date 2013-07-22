/* Check that the GBR address optimization does not combine a gbr store
   and its use when a function call is in between, when GBR is a call used
   register, i.e. it is invalidated by function calls.  */
/* { dg-do compile { target "sh*-*-*" } } */
/* { dg-options "-O1 -fcall-used-gbr" } */
/* { dg-skip-if "" { "sh*-*-*" } { "-m5*"} { "" } }  */
/* { dg-final { scan-assembler "stc\tgbr" } } */

extern int test00 (void);
int
test01 (int x)
{
  /* We must see a stc gbr,rn before the function call, because
     a function call could modify the gbr.  In this case the user requests
     the old gbr value, before the function call.  */
  int* p = (int*)__builtin_thread_pointer ();
  p[5] = test00 ();
  return 0;
}
