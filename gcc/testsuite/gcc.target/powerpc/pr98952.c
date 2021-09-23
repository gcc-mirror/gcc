/* { dg-do run { target { powerpc*-*-linux* && ilp32 } } } */
/* { dg-options "-O2" } */

/* PR 96983 reported that the test in libgcc's tramp.S was backwards and it
   would abort if the trampoline size passed to the function was greater than
   the size the runtime was expecting (40).  It should abort if the size is less
   than 40, not greater than 40.  This test creates a call to __trampoline_setup
   with a much larger buffer to make sure the function does not abort.

   We do not run this test on 64-bit since __trampoline_setup is not present in
   64-bit systems.

   We only run the test under Linux in case the other systems have some
   different variant for __trampoline_setup.  */

#ifndef SIZE
#define SIZE 100
#endif

extern void __trampoline_setup (int *, unsigned, void *, void *);

int main (void)
{
  int tramp[SIZE / sizeof (int)];

  __trampoline_setup (tramp, SIZE, 0, 0);
  return 0;
}
