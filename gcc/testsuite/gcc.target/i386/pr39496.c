/* PR target/39496 */
/* { dg-do compile { target { { i?86-*-linux* x86_64-*-linux* } && ilp32 } } } */
/* { dg-options "-O0 -fverbose-asm -fno-omit-frame-pointer -mtune=i686 -msse2 -mfpmath=sse" } */
/* { dg-require-effective-target sse2 } */
/* Verify that {foo,bar}{,2}param are all passed on the stack, using
   normal calling conventions, when not optimizing.  */
/* { dg-final { scan-assembler "\[^0-9-\]8\\(%ebp\\),\[^\n\]*fooparam," } } */
/* { dg-final { scan-assembler "\[^0-9-\]8\\(%ebp\\),\[^\n\]*barparam," } } */
/* { dg-final { scan-assembler "\[^0-9-\]8\\(%ebp\\),\[^\n\]*foo2param," } } */
/* { dg-final { scan-assembler "\[^0-9-\]8\\(%ebp\\),\[^\n\]*bar2param," } } */

static inline int foo (int fooparam)
{
  return fooparam;
}

static int bar (int barparam)
{
  return foo (barparam);
}

static inline double foo2 (double foo2param)
{
  return foo2param;
}

static double bar2 (double bar2param)
{
  return foo2 (bar2param);
}

int
main ()
{
  return bar (0) + bar2 (0.0);
}
