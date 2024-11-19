/* PR middle-end/117459 */
/* { dg-do run { target bitint } } */
/* { dg-options "-std=c23" } */
/* { dg-skip-if "" { ! run_expensive_tests }  { "*" } { "-O0" "-O2" } } */
/* { dg-skip-if "" { ! run_expensive_tests } { "-flto" } { "" } } */

#if __BITINT_MAXWIDTH__ >= 255
_BitInt(255) b, c, d;

_BitInt(255)
foo ()
{
  return __builtin_assoc_barrier (b + c) + d;
}
#endif

int
main ()
{
#if __BITINT_MAXWIDTH__ >= 255
  b = 3162082328713384445049140446737468449630746270013462291267283007210433157591wb;
  c = 12998515555477887328635550261966833804427562203752161174274777867442907371807wb;
  d = 5016523343681809792116154509287659112784399275423992541459788346980443294044wb;
  if (foo () != 21177121227873081565800845217991961366842707749189616007001849221633783823442wb)
    __builtin_abort ();
#endif
}
