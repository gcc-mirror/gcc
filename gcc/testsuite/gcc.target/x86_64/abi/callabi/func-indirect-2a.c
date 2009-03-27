/* Test for cross x86_64<->w64 abi standard calls via variable.  */
/* { dg-do run { target i?86-*-linux* x86_64-*-linux* } } */
/* { dg-options "-O2 -mabi=ms -std=gnu99 -ffast-math -fno-builtin" } */
/* { dg-additional-sources "func-indirect-2b.c" } */

extern void __attribute__ ((sysv_abi)) abort (void);
typedef int (*func)(void *, char *, char *, short, long long);
extern func get_callback (void);

int __attribute__ ((sysv_abi))
main ()
{
  func callme = get_callback ();
  if (callme (0, 0, 0, 0x1234, 0x1234567890abcdefLL))
    abort ();
  return 0;
}
