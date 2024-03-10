/* { dg-do compile } */
/* { dg-options "-O -fshrink-wrap" } */

/* We should not save anything before checking the value of x.  */
/* { dg-final { scan-assembler-not "st(ptr)?\\\.\[dw\].*b(eq|ne)z" } } */

int
foo (int x)
{
  __asm__ ("nop" :);
  if (x)
    {
      __asm__ ("" ::: "s0", "s1");
      return x;
    }

  __asm__ ("" ::: "s2", "s3");
  return 0;
}
