/* Test -mnop-mcount */
/* { dg-do compile { target { *-*-linux* && nonpic } } } */
/* { dg-options "-pg -mfentry -mrecord-mcount -mnop-mcount" } */
/* { dg-final { scan-assembler-not "__fentry__" } } */
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

void func3(a)
char *a;
{
  foobar("Hello world");
}
