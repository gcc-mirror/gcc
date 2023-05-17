/* { dg-do compile { target int128 } } */
/* { dg-options "-O -std=c11" } */
/* { dg-final { scan-assembler-not {abort} } } */

/* The stack is 8 byte aligned which means GCC has to manually align a 16 byte
   aligned object.  This is done by allocating not 16 but rather 24 bytes for
   variable X and then manually aligning a pointer inside the memory block.
   Validate this by ensuring that the if-statement is optimized out.  */

void bar (_Atomic unsigned __int128 *ptr);

void foo (void) {
  _Atomic unsigned __int128 x;
  unsigned long n = (unsigned long)&x;
  if (n % 16 != 0)
    __builtin_abort ();
  bar (&x);
}
