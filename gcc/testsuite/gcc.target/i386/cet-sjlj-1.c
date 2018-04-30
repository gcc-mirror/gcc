/* { dg-do compile } */
/* { dg-options "-O -fcf-protection" } */
/* { dg-final { scan-assembler-times "endbr32" 4 { target ia32 } } } */
/* { dg-final { scan-assembler-times "endbr64" 4 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "rdssp\[dq]" 2 } } */
/* { dg-final { scan-assembler-times "incssp\[dq]" 2 } } */

/* Based on gcc.dg/setjmp-3.c.  */

void *buf[5];

extern void abort (void);

void raise0(void)
{
  __builtin_longjmp (buf, 1);
}

int execute(int cmd)
{
  int last = 0;

  if (__builtin_setjmp (buf) == 0)
    while (1)
      {
	last = 1;
	raise0 ();
      }

  if (last == 0)
    return 0;
  else
    return cmd;
}

int main(void)
{
  if (execute (1) == 0)
    abort ();

  return 0;
}
