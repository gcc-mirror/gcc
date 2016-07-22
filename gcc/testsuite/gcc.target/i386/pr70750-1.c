/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2" } */

int
f (int (**p) (void))
{
  return p[1]();
}

/* { dg-final { scan-assembler "jmp\[ \t\].*\\(%rdi\\)" { target { lp64 } } } } */
/* { dg-final { scan-assembler "jmp\[ \t\]\\*%rax" { target { x32 } } } } */
