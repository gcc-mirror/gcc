/* Test 'w' and 'W' suffixes on __float80 type constants.  */
/* { dg-do compile { target i?86-*-* x86_64-*-* ia64-*-* } } */
/* { dg-options "" } */
/* { dg-options "-mmmx" { target { { i?86-*-* x86_64-*-* } && ilp32 } } } */

__float80 a = 123.456789W;
__float80 b = 123.456789w;
