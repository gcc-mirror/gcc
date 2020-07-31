/* { dg-do preprocess { target c } } */
/* { dg-options "-traditional-cpp" } */

#if __has_include ("stdlib.h")
#else
#error error 1
#endif
#if __has_include (<stdlib.h>)
#else
#error error 2
#endif
#if !__has_include ("stdlib.h")
#error error 3
#elif !__has_include (<stdlib.h>)
#error error 4
#endif
#if __has_include ("stdlib.h") && __has_include (<stdlib.h>)
#else
#error error 5
#endif
#if !defined(__has_include)
#error error 6
#endif
#ifndef __has_include
#error error 7
#endif
#ifdef __has_include
#else
#error error 8
#endif
#define m1 __has_include("stdlib.h")
#define m2 <stdlib.h>
#if !m1
#error error 9
#endif
#if !__has_include (m2)
#error error 13
#endif
