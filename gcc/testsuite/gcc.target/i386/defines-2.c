/* { dg-do compile } */
/* { dg-options "-march=athlon64 -mno-mmx" } */

#if defined(__MMX__) || defined(__3dNOW__) || defined(__3dNOW_A__)
#error
#endif
