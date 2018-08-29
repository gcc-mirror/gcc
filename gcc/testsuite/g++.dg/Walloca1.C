/* PR middle-end/79809 - ICE in alloca_call_type, at gimple-ssa-warn-alloca.c */
/* { dg-do compile } */
/* { dg-options "-Walloca-larger-than=4207115063 -Wvla-larger-than=1233877270 -O2" } */
/* { dg-require-effective-target alloca } */

int a;
char *b = static_cast<char *>(__builtin_alloca (a));

// { dg-prune-output "argument to .alloca." }
