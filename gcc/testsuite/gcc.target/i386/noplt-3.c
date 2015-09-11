/* { dg-do compile { target { *-*-linux* && { ! ia32 } } } } */
/* { dg-options "-fno-pic -fno-plt" } */

void foo();

int main()
{
  foo();
  return 0;
}

/* { dg-final { scan-assembler "call\[ \t\]\\*.*foo.*@GOTPCREL\\(%rip\\)" } } */ 
