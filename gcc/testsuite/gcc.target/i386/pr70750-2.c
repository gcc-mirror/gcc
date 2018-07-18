/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2" } */

int
f (int (**p) (void))
{
  return -p[1]();
}

/* { dg-final { scan-assembler "call\[ \t\].*\\(%rdi\\)" { target lp64 } } } */
/* { dg-final { scan-assembler "call\[ \t\]\\*%rax" { target x32 } } } */
