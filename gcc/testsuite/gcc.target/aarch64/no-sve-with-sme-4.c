/* { dg-do compile } */
/* { dg-skip-if "Do not override mcpu or march" { *-*-* } { -mcpu=* -march=* } { "" } } */
/* { dg-options { "-march=armv8-a" } } */
/* { dg-message "sorry, unimplemented: no support for 'sme' without 'sve2'" "" { target *-*-* } 0 } */

#pragma GCC target "+sme"

int main (void)
{
  return 0;
}
