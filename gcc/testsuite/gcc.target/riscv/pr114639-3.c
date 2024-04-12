/* Test that we do not have ice when compile */
/* { dg-do compile } */
/* { dg-options "-march=rv32gc -mabi=ilp32d -std=gnu89 -O3" } */

g (a, b) {}

f (xx)
     void* xx;
{
  __builtin_apply ((void*)g, xx, 200);
}
