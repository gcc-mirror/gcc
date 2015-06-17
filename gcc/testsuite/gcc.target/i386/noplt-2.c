/* { dg-do compile { target { *-*-linux* && { ! ia32 } } } } */
/* { dg-options "-O2 -fno-pic" } */


__attribute__ ((noplt))
int foo();

int main()
{
  return foo();
}

/* { dg-final { scan-assembler "jmp\[ \t\]\\*.*foo.*@GOTPCREL\\(%rip\\)" } } */ 
