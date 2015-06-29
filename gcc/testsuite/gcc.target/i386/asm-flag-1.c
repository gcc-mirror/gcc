/* Test some of the valid @cc<cc> asm flag outputs.  */
/* { dg-do compile } */
/* { dg-options "-O" } */

void f(char *out)
{
  asm("" : "=@cca"(out[0]), "=@ccc"(out[1]), "=@cce"(out[2]),
           "=@ccg"(out[3]), "=@cco"(out[4]), "=@ccp"(out[5]),
           "=@ccs"(out[6]));
}

/* { dg-final { scan-assembler "seta" } } */
/* { dg-final { scan-assembler "setc" } } */
/* { dg-final { scan-assembler "sete" } } */
/* { dg-final { scan-assembler "setg" } } */
/* { dg-final { scan-assembler "seto" } } */
/* { dg-final { scan-assembler "setp" } } */
/* { dg-final { scan-assembler "sets" } } */
