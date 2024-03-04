/* PR middle-end/107385 */
/* { dg-do run { target asm_goto_with_outputs } } */
/* { dg-options "-O2" } */

__attribute__((noipa)) int
foo (void)
{
  int x;
  asm goto ("": "=r" (x) : "0" (15) :: lab);
  x = 6;
lab:
  return x;
}

int
main ()
{
  if (foo () != 6)
    __builtin_abort ();
}
