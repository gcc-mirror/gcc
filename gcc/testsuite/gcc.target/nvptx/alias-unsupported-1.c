/* { dg-do compile } */
/* { dg-additional-options -mno-alias } */

void __f ()
{
}

void f () __attribute__ ((alias ("__f")));
/* { dg-error {alias definitions not supported} {} { target *-*-* } .-1 } */
