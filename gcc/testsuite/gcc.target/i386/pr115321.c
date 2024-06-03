/* { dg-do compile { target ia32 } } */
/* { dg-options "-march=i386" } */

unsigned foo (unsigned x) { return __builtin_bswap32 (x); }
