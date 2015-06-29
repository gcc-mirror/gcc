/* Test some of the valid @cc<cc> asm flag outputs.  */
/* { dg-do compile } */
/* { dg-options "-O" } */

void f(char *out)
{
  asm("" : "=@ccb"(out[0]), "=@ccl"(out[1]), "=@ccz"(out[2]),
           "=@ccbe"(out[4]), "=@ccge"(out[5]), "=@ccle"(out[6]));
}

/* { dg-final { scan-assembler "setc" } } */
/* { dg-final { scan-assembler "setl" } } */
/* { dg-final { scan-assembler "sete" } } */
/* { dg-final { scan-assembler "setna" } } */
/* { dg-final { scan-assembler "setge" } } */
/* { dg-final { scan-assembler "setle" } } */
