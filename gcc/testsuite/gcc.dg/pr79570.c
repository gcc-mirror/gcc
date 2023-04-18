/* PR target/79570 */
/* { dg-do compile { target powerpc*-*-* ia64-*-* i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -fselective-scheduling2 -fvar-tracking-assignments -g" } */
/* { dg-warning "changes selective scheduling" "" { target *-*-* } 0 } */

#include "pr69956.c"
