/* { dg-do compile } */
/* { dg-options "-march=nocona -mno-sse" } */

#if defined(__SSE__) || defined(__SSE2__) || defined(__SSE3__)
#error
#endif
