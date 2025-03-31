/* { dg-do compile } */
/* { dg-message "sorry, unimplemented: no support for 'sme' without 'sve2'" "" { target *-*-* } 0 } */

#pragma GCC target ("arch=armv8.2-a+ssve-fp8fma")

int main (void)
{
  return 0;
}
