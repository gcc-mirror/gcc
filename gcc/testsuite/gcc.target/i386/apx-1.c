/* { dg-do compile } */
/* { dg-options "-O2 -mapxf" } */
/* { dg-error "'-mapxf' is not supported for 32-bit code" "" { target ia32 } 0 } */

void
apx_hanlder ()
{
}
