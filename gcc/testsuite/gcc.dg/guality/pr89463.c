/* { dg-do run } */
/* { dg-options "-g" } */

void __attribute__((noinline))
optimize_me_not ()
{
  __asm__ volatile ("" : : : "memory");
}
int a;
int main()
{
  int i;
  for (; a < 10; a++)
    i = 0;
  for (; i < 6; i++)
    ;
  /* i may very well be optimized out, so we cannot test for i == 6.
     Instead test i + 1 which will make the test UNSUPPORTED if i
     is optimized out.  Since the test previously had wrong debug
     with i == 0 this is acceptable.  Optimally we'd produce a
     debug stmt for the final value of the loop which would fix
     the UNSUPPORTED cases.  */
  optimize_me_not(); /* { dg-final { gdb-test . "i + 1" "7" } } */
  return 0;
}
