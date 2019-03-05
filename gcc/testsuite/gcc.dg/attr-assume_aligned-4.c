/* PR middle-end/87533 - bogus assume_aligned attribute silently accepted
   { dg-do compile }
   { dg-options "-Wall" } */

#define A(...)  __attribute__ ((assume_aligned (__VA_ARGS__)))

A (1) void fv_1 (void);       /* { dg-warning ".assume_aligned. attribute ignored on a function returning .void." } */

A (1) int fi_1 (void);        /* { dg-warning ".assume_aligned. attribute ignored on a function returning .int." } */

A (-1) void* fpv_m1 (void);   /* { dg-warning ".assume_aligned. attribute argument -1 is not a power of 2" } */

A (0) void* fpv_0 (void);     /* { dg-warning ".assume_aligned. attribute argument 0 is not a power of 2" } */

/* Alignment of 1 is fine, it just doesn't offer any benefits.  */
A (1) void* fpv_1 (void);

A (3) void* fpv_3 (void);     /* { dg-warning ".assume_aligned. attribute argument 3 is not a power of 2" } */

A (16383) void* fpv_16km1 (void);     /* { dg-warning ".assume_aligned. attribute argument 16383 is not a power of 2" } */
A (16384) void* fpv_16k (void);
A (16385) void* fpv_16kp1 (void);    /* { dg-warning ".assume_aligned. attribute argument 16385 is not a power of 2" } */

A (32767) void* fpv_32km1 (void);     /* { dg-warning ".assume_aligned. attribute argument 32767 is not a power of 2" } */

A (4, -1) void* fpv_4_m1 (void);      /* { dg-warning ".assume_aligned. attribute argument -1 is not in the range \\\[0, 4\\\)" } */

A (4, 0) void* fpv_4_0 (void);
A (4, 1) void* fpv_4_1 (void);
A (4, 2) void* fpv_4_2 (void);
A (4, 3) void* fpv_4_3 (void);

A (4, 4) void* fpv_4_3 (void);        /* { dg-warning ".assume_aligned. attribute argument 4 is not in the range \\\[0, 4\\\)" } */

A (4) void* gpv_4_3 (void);
A (2) void* gpv_4_3 (void);
