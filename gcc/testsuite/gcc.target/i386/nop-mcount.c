/* Test -mnop-mcount */
/* { dg-do compile { target { *-*-linux* && nonpic } } } */
/* { dg-require-effective-target mfentry } */
/* { dg-options "-pg -mfentry -mrecord-mcount -mnop-mcount" } */
/* { dg-final { scan-assembler-not "__fentry__" } } */
/* { dg-final { scan-assembler-times "0x0f, 0x1f, 0x44, 0x00, 0x00" 3 } } */
/* Origin: Andi Kleen */
extern void foobar(char *);

void func(void)
{
  foobar ("Hello world\n");
}

void func2(void)
{
  int i;
  for (i = 0; i < 10; i++)
    foobar ("Hello world");
}

void func3(char *a)
{
  foobar("Hello world");
}
