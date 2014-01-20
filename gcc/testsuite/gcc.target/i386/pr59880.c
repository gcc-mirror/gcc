/* PR target/59880 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mtune=silvermont" } */

register unsigned int r13 __asm ("r13");
unsigned long long
foo (void)
{
  return r13;
}

/* Ensure we don't emit a useless zero-extension after another
   zero-extension.  */
/* { dg-final { scan-assembler-not "%eax, %eax" } } */
