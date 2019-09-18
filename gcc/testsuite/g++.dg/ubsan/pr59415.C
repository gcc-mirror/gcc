/* { dg-do compile } */
/* { dg-options "-fsanitize=null -Wall -fvtable-verify=std -fno-lto" } */

void
foo (void)
{
  throw 0;
}
