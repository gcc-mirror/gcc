/* { dg-do compile { target x32 } } */
/* { dg-options "-mabi=ms" } */
/* { dg-error "-mabi=ms not supported with X32 ABI" "" { target *-*-* } 0 } */

void main ()
{
}
