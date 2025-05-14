/* { dg-do compile } */
/* { dg-skip-if "Do not override mcpu or march" { *-*-* } { -mcpu=* -march=* } { "" } } */
/* { dg-options { "-march=armv8-a+sme" } } */
/* { dg-message "sorry, unimplemented: no support for 'sme' without 'sve2'" "" { target *-*-* } 0 } */
int main (void)
{
  return 0;
}
