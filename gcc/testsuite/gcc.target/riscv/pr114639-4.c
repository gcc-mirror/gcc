/* Test that we do not have ice when compile */
/* { dg-do compile } */
/* { dg-options "-march=rv32imac -mabi=ilp32 -std=gnu89 -O3" } */

g (a, b) {}

f (xx)
     void* xx;
{
  __builtin_apply ((void*)g, xx, 200);
}
