/* Test -mrecord-mcount */
/* { dg-do compile { target { *-*-linux* && nonpic } } } */
/* { dg-options "-pg -mrecord-mcount" } */
/* { dg-final { scan-assembler "mcount_loc" } } */
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
