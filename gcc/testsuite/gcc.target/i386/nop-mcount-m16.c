/* Test -mnop-mcount for 16-bit code to generate a 3-byte NOP */
/* { dg-do compile { target { *-*-linux* && nonpic } } } */
/* { dg-require-effective-target mfentry } */
/* { dg-options "-pg -mfentry -mnop-mcount -m16" } */
/* { dg-final { scan-assembler-not "__fentry__" } } */
/* { dg-final { scan-assembler-not "0x0f, 0x1f, 0x44, 0x00, 0x00" } } */
/* { dg-final { scan-assembler-times "0x8d, 0x74, 0x00" 1 } } */
extern void foobar(char *);

void func(void)
{
  foobar ("Hello world\n");
}
