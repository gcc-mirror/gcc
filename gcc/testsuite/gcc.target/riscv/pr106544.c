/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-flto"} } */
void f(int a)
{
  __asm("#%A0" : : "r"(a)); /* { dg-error "invalid 'asm': invalid operand" } */
}
