/* { dg-do compile { target { ! ia32 } } } */
/* { dg-require-effective-target mfentry } */
/* { dg-require-profiling "-pg" } */
/* { dg-options "-pg -mfentry -minstrument-return=call" } */
/* { dg-final { scan-assembler-not "call.*__return__" } } */

__attribute__((no_instrument_function))
int func(int a)
{
  return a+1;
}
