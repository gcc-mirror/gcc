/* { dg-do compile } */
/* { dg-options "-O" } */

struct Reg_T {
    unsigned int a : 3;
    unsigned int b : 1;
    unsigned int c : 4;
};

volatile struct Reg_T Reg_A;

int
main ()
{
  Reg_A = (struct Reg_T){ .a = 0, .b = 0, .c = 0 };
  return 0;
}

/* { dg-final { scan-assembler-times "mov\[^\r\n\]*Reg_A" 1 } } */
