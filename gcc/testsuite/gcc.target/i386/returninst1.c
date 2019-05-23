/* { dg-do compile { target { ! ia32 } } } */
/* { dg-require-effective-target mfentry } */
/* { dg-options "-pg -mfentry -minstrument-return=call -mrecord-return" } */
/* { dg-final { scan-assembler "call.*__return__" } } */
/* { dg-final { scan-assembler "section.*return_loc" } } */

int func(int a)
{
  return a+1;
}

int func2(int a)
{
  return a+1;
}
