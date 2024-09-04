/* { dg-do compile } */
/* { dg-add-options ptx_alias } */

void __f ()
{
}

void f () __attribute__ ((weak, alias ("__f")));
/* { dg-error {weak alias definitions not supported} {} { target *-*-* } .-1 }
   (limitation of PTX).  */
