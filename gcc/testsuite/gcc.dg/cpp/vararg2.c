/* { dg-do preprocess } */
/* { dg-options "-std=gnu99" } */

/* Jamie's varargs macros from hell.  Not for the faint of heart.
   Great tests that C99 and GNU varargs give identical results.
   Adapted to the testsuite by Neil Booth, 1 Nov 2000.  */

/* Permission is granted to use, copy, modify and distribute this file
   freely for any purpose whatsoever.  This file is free software, and
   there's no warranty.

   -- Jamie Lokier <jamie.lokier@cern.ch>, 25/Sep/2000.  */

#define dup3(x)            x,x,x

/* Count elements in a list (0 to 10 max). */
#define gnu_count(y...)   _gnu_count1 ( , ##y)
#define _gnu_count1(y...) _gnu_count2 (y,10,9,8,7,6,5,4,3,2,1,0)
#define _gnu_count2(_,x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,n,ys...) n

/* Tail of a list. */
#define gnu_tail(y...)    _gnu_tail (y)
#define _gnu_tail(x,y...) y

/* Repeat N times. */
#define gnu_repeat(n, x) gnu_tail (_gnu_repeat (n, x))
#define _gnu_repeat(n, x) _gnu_repeat_##n (x)
#define _gnu_repeat_0(x)
#define _gnu_repeat_1(x) ,x
#define _gnu_repeat_2(x) ,x,x
#define _gnu_repeat_3(x) ,x,x,x
#define _gnu_repeat_4(x) ,x,x,x,x
#define _gnu_repeat_5(x) ,x,x,x,x,x

#define _gnu_keep(xs...) xs
#define _gnu_discard(xs...)
#define _gnu_split_r(n,xs...) _gnu_split_rd (n,_gnu_keep,_gnu_discard xs)
#define _gnu_split_d(n,xs...) _gnu_split_rd (n,_gnu_discard,_gnu_keep xs)
#define _gnu_split_rd(n,xs...) _gnu_split_##n (xs)
#define _gnu_split_0(a,b,xs...) a() b(xs)
#define _gnu_split_1(a,b,x0,xs...) a(x0) b(xs)
#define _gnu_split_2(a,b,x0,x1,xs...) a(x0,x1) b(xs)
#define _gnu_split_3(a,b,x0,x1,x2,xs...) a(x0,x1,x2) b(xs)
#define _gnu_split_4(a,b,x0,x1,x2,x3,xs...) a(x0,x1,x2,x3) b(xs)
#define _gnu_split_5(a,b,x0,x1,x2,x3,x4,xs...) a(x0,x1,x2,x3,x4) b(xs)

/* List manipulations.  Surprise: index zero is the rightmost element. */
#define gnu_take(n, xs...) \
  _gnu_split_d (_gnu_count1 ( , ## xs), _gnu_repeat (n, _gnu_error) , ## xs)
#define gnu_drop(n, xs...) \
  _gnu_split_d (n,,_gnu_split_r  (_gnu_count1 ( , ## xs), _gnu_repeat (n, _gnu_error) , ## xs))
#define gnu_index(pos, xs...) gnu_take (1, gnu_drop (pos , ## xs))

/* C99 __VA_ARGS__ versions */
#define c99_count(...)    _c99_count1 ( , ##__VA_ARGS__)/* If only ## worked.*/
#define _c99_count1(...)  _c99_count2 (__VA_ARGS__,10,9,8,7,6,5,4,3,2,1,0)
#define _c99_count2(_,x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,n,...) n

#define c99_tail(...)     _c99_tail (__VA_ARGS__)
#define _c99_tail(x,...)  __VA_ARGS__

/* Repeat N times. */
#define c99_repeat(n, x) c99_tail (_c99_repeat (n, x))
#define _c99_repeat(n, x) _c99_repeat_##n (x)
#define _c99_repeat_0(x)
#define _c99_repeat_1(x) ,x
#define _c99_repeat_2(x) ,x,x
#define _c99_repeat_3(x) ,x,x,x
#define _c99_repeat_4(x) ,x,x,x,x
#define _c99_repeat_5(x) ,x,x,x,x,x

#define _c99_keep(...)    __VA_ARGS__
#define _c99_discard(...)
#define _c99_split_r(n,...) _c99_split_rd(n,_c99_keep,_c99_discard __VA_ARGS__)
#define _c99_split_d(n,...) _c99_split_rd(n,_c99_discard,_c99_keep __VA_ARGS__)
#define _c99_split_rd(n,...) _c99_split_##n (__VA_ARGS__)
#define _c99_split_0(a,b,...) a() b(__VA_ARGS__)
#define _c99_split_1(a,b,x0,...) a(x0) b(__VA_ARGS__)
#define _c99_split_2(a,b,x0,x1,...) a(x0,x1) b(__VA_ARGS__)
#define _c99_split_3(a,b,x0,x1,x2,...) a(x0,x1,x2) b(__VA_ARGS__)
#define _c99_split_4(a,b,x0,x1,x2,x3,...) a(x0,x1,x2,x3) b(__VA_ARGS__)
#define _c99_split_5(a,b,x0,x1,x2,x3,x4,...) a(x0,x1,x2,x3,x4) b(__VA_ARGS__)

/* List manipulations.  Surprise: index zero is the rightmost element. */
#define c99_take(n, ...) \
  _c99_split_d (_c99_count1 ( , ## __VA_ARGS__), _c99_repeat (n, _c99_error) , ## __VA_ARGS__)
#define c99_drop(n, ...) \
  _c99_split_d (n,,_c99_split_r  (_c99_count1 ( , ## __VA_ARGS__), _c99_repeat (n, _c99_error) , ## __VA_ARGS__))
#define c99_index(pos, ...) c99_take (1, c99_drop (pos , ## __VA_ARGS__))

/************** Expansions **************/

/* Correct answers are 0, 0, 1, 2, 10.  */
#if _gnu_count1 () != 0 || gnu_count () != 0 || gnu_count (A) != 1 \
    || gnu_count (,) != 2 || gnu_count (A, B, C, D, E, F, G, H, I, J) != 10
#error gnu_count
#endif

/* Correct answers are empty, 'x'.  */
#if gnu_repeat (0, 'x') gnu_repeat (1, 'x') != 'x'
#error gnu_repeat
#endif

/* Correct answers are "e", "b", "a", empty.  */
#if gnu_index (0, 'a', 'b', 'c', 'd', 'e') != 'e' \
 || gnu_index (3, 'a', 'b', 'c', 'd', 'e') != 'b' \
 || gnu_index (4, 'a', 'b', 'c', 'd', 'e') != 'a' \
    gnu_index (5, 'a', 'b', 'c', 'd', 'e')
#error gnu_index
#endif

/************* C99 tests *************/

/* The answers are 0, 0, 1, 2, 10 as for the non-C99 version.  */
#if _c99_count1 () != 0 || c99_count () != 0 || c99_count (A) != 1 \
    || c99_count (,) != 2 || c99_count (A, B, C, D, E, F, G, H, I, J) != 10
#error c99_count
#endif

/* Correct answers are empty, 'x'.  */
#if c99_repeat (0, 'x') c99_repeat (1, 'x') != 'x'
#error c99_repeat
#endif

/* Correct answers are "e", "b", "a", empty.  */
#if c99_index (0, 'a', 'b', 'c', 'd', 'e') != 'e' \
 || c99_index (3, 'a', 'b', 'c', 'd', 'e') != 'b' \
 || c99_index (4, 'a', 'b', 'c', 'd', 'e') != 'a' \
    c99_index (5, 'a', 'b', 'c', 'd', 'e')
#error gnu_index
#endif
