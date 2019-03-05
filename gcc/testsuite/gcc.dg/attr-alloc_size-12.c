/* PR middle-end/81871 - bogus attribute alloc_align accepted
   Test exercising the problem with attribute alloc_size.
   { dg-do compile }
   { dg-options "-Wall" } */

#define ASIZE(...)   __attribute__ ((alloc_size (__VA_ARGS__)))
#define SIZE_MAX  __SIZE_MAX__

ASIZE (-1) void fvv_m1 (void);    /* { dg-warning ".alloc_size. attribute ignored on a function returning .void." } */

ASIZE (1) int fiv_1 (void);       /* { dg-warning ".alloc_size. attribute ignored on a function returning .int." } */

ASIZE (1, 2) int fiv_1_2 (void);  /* { dg-warning ".alloc_size. attribute ignored on a function returning .int." } */

ASIZE (0) void* fpvv_0 (void);    /* { dg-warning ".alloc_size. attribute argument value .0. does not refer to a function parameter" } */

ASIZE (1, 0) void*
fpvv_1_0 (int);                   /* { dg-warning ".alloc_size. attribute argument 2 value .0. does not refer to a function parameter" } */

ASIZE (1) void* fpvv_1 (void);    /* { dg-warning ".alloc_size. attribute argument value .1. exceeds the number of function parameters 0" } */

ASIZE (1, 9) void*
fpvv_1_9 (int);                   /* { dg-warning ".alloc_size. attribute argument 2 value .9. exceeds the number of function parameters 1" } */

ASIZE (2) void* fii_2 (int);      /* { dg-warning ".alloc_size. attribute argument value .2. exceeds the number of function parameters 1" } */

ASIZE (1) void fvi_1 (int);       /* { dg-warning ".alloc_size. attribute ignored on a function returning .void." } */

/* Using alloc_size with a function returning a pointer to a function
   should perhaps trigger a warning.  */
typedef void (F)(void);
ASIZE (1) F* fpF_i_1 (int);

ASIZE (SIZE_MAX) void*
fpvi_szmax (int);                 /* { dg-warning ".alloc_size. attribute argument value .\[0-9\]+. exceeds the number of function parameters 1" } */

ASIZE ("12") void*
fpvi_str_1 (int);                 /* { dg-warning ".alloc_size. attribute argument has type .char\\\[3]." } */

ASIZE (1, "123") void*
fpvi_str_2 (int, int);            /* { dg-warning ".alloc_size. attribute argument 2 has type .char\\\[4]." } */

ASIZE ((void*)0) void*
fpvi_pv0 (int);                   /* { dg-warning ".alloc_size. attribute argument has type .void \\\*." } */

ASIZE ((double*)sizeof (double)) void*
fpvi_pd1 (int);                   /* { dg-warning ".alloc_size. attribute argument has type .double \\\*." } */

ASIZE (1) void*
fpvi_pv_1 (void*);                /* { dg-warning ".alloc_size. attribute argument value .1. refers to parameter type .void \\\*." } */

struct S { int i; };
ASIZE (2) void*
fpvi_S_2 (int, struct S);         /* { dg-warning ".alloc_size. attribute argument value .2. refers to parameter type .struct S." } */

ASIZE ((struct S){ 1 }) void*
fpvi_S (int);                     /* { dg-warning ".alloc_size. attribute argument has type .struct S." } */

ASIZE (1, (struct S){ 1 }) void*
fpvi_1_S (int);                   /* { dg-warning ".alloc_size. attribute argument 2 has type .struct S." } */
