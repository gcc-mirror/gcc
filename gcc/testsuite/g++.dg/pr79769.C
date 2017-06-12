/* { dg-do compile { target { { i?86-*-* x86_64-*-* } && { ! x32 } } } } */
/* { dg-options "-fcheck-pointer-bounds -mmpx -mabi=ms" } */

void a (_Complex) { a (3); }
