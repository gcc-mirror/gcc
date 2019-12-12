// PR c++/88636
// { dg-do compile }
// { dg-options "-msse2 -mno-sse3 -fno-exceptions --param ggc-min-heapsize=0" }

extern unsigned int __builtin_ia32_crc32si (unsigned int, unsigned int);
#pragma GCC target("sse4.2")
