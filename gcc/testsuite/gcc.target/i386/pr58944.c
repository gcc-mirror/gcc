/* { dg-do compile } */
/* { dg-options "-Wunused-macros" } */

#pragma GCC push_options
#pragma GCC target("xsaveopt")
void fn1(void) {}
#pragma GCC pop_options
