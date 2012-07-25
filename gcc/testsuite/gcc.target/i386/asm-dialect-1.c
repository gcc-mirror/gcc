/* { dg-options "-masm=intel" } */

extern void abort (void);

int
main (void)
{
  int f = 0;
  asm ("{movl $42, %%eax | mov eax, 42}" : :);
  asm ("{movl $41, %0||mov %0, 43}" : "=r"(f));
  if (f != 42)
    abort ();

  return 0;
}
