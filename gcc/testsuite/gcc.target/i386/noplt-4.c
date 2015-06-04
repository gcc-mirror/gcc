/* { dg-do compile { target x86_64-*-linux* } } */
/* { dg-options "-O2 -fno-pic -fno-plt" } */

int foo();

int main()
{
  return foo();
}

/* { dg-final { scan-assembler "jmp\[ \t\]\\*.*foo.*@GOTPCREL\\(%rip\\)" } } */
