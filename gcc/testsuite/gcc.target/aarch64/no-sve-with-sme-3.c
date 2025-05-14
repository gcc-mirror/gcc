/* { dg-do compile } */
/* { dg-message "sorry, unimplemented: no support for 'sme' without 'sve2'" "" { target *-*-* } 0 } */

int __attribute__ ((target( "arch=armv8.2-a+ssve-fp8fma"))) main (void)
{
  return 0;
}

