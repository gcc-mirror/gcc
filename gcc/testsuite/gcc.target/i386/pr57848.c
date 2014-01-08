/* { dg-do compile } */
/* { dg-options "-O1" } */

extern unsigned int __builtin_ia32_crc32si (unsigned int, unsigned int);
#pragma GCC target("sse4.2")

