/* { dg-do compile } */
/* { dg-options "-Wunused-macros -march=native" } */

#pragma GCC push_options
#pragma GCC target("xsaveopt")
void fn1(void) {}
#pragma GCC pop_options

/* { dg-prune-output "macro \"__code_model_" } */ 
/* { dg-prune-output "macro \"__XSAVE__\" is not used" } */ 
/* { dg-prune-output "macro \"__XSAVEOPT__\" is not used" } */ 
