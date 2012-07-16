/* Check that the basic library call variant is sane; no other calls, no
   checks compares or branches.  */
/* { dg-do compile } */
/* { dg-options "-O2 -munaligned-atomic-may-use-library" } */
/* { dg-final { scan-assembler-not "\tdi" } } */
/* { dg-final { scan-assembler-not "\tbtstq" } } */
/* { dg-final { scan-assembler-not "\tand" } } */
/* { dg-final { scan-assembler-not "\tclearf" } } */
/* { dg-final { scan-assembler-not "\tmove.d" } } */
/* { dg-final { scan-assembler-not "\tcmp" } } */
/* { dg-final { scan-assembler-not "\tb\[^s\]" } } */
/* { dg-final { scan-assembler-times "\t\[JjBb\]sr" 1 } } */

#ifndef type
#define type int
#endif

type svcsw (type *ptr, type oldval, type newval)
{
  return __sync_val_compare_and_swap (ptr, oldval, newval);
}
