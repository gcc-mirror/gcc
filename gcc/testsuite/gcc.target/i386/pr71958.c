/* { dg-do compile } */
/* { dg-require-effective-target maybe_x32 } */
/* { dg-options "-mx32 -mabi=ms" } */
/* { dg-error "'-mabi=ms' not supported with X32 ABI" "" { target *-*-* } 0 } */

void main ()
{
}
