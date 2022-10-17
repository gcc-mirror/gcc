/* PR debug/102585 */
/* { dg-do compile } */
/* { dg-options "-fvar-tracking-assignments -fno-var-tracking" } */

#pragma GCC optimize 0
void d_demangle_callback_Og() { int c = 0; }
