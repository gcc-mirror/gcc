/* Test that we do not have ice when compile */
/* { dg-do compile } */
/* { dg-options "-march=rv64imac -mabi=lp64 -std=gnu89 -O3" } */

g (a, b) {}

f (xx)
     void* xx;
{
  __builtin_apply ((void*)g, xx, 200);
}
