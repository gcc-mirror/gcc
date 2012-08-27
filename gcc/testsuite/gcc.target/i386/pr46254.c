/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target fpic } */
/* { dg-options "-O2 -mcx16 -fpic -mcmodel=large" } */

__int128 i;

void test ()
{
  __sync_val_compare_and_swap (&i, i, i);
}
