/* PR middle-end/50907 */
/* { dg-require-effective-target freorder } */
/* { dg-options "-O2 -freorder-blocks-and-partition -fschedule-insns -fselective-scheduling -fpic" { target { { powerpc*-*-* ia64-*-* i?86-*-* x86_64-*-* } && fpic } } } */

#include "pr45354.c"
