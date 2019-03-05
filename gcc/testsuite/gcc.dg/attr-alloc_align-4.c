/* PR middle-end/81871 - bogus attribute alloc_align accepted
   { dg-do compile }
   { dg-options "-Wall" } */

#define ALIGN(N)   __attribute__ ((alloc_align (N)))
#define SIZE_MAX  __SIZE_MAX__

ALIGN (1) void fvv_m1 (void);     /* { dg-warning ".alloc_align. attribute ignored on a function returning .void." } */

ALIGN (1) int fiv_1 (void);       /* { dg-warning ".alloc_align. attribute ignored on a function returning .int." } */

ALIGN (0) void* fpvv_0 (void);    /* { dg-warning ".alloc_align. attribute argument value .0. does not refer to a function parameter" } */

ALIGN (1) void* fpvv_1 (void);    /* { dg-warning ".alloc_align. attribute argument value .1. exceeds the number of function parameters 0" } */

ALIGN (2) void* fii_2 (int);      /* { dg-warning ".alloc_align. attribute argument value .2. exceeds the number of function parameters 1" } */

ALIGN (1) void fvi_1 (int);       /* { dg-warning ".alloc_align. attribute ignored on a function returning .void." } */

/* Using alloc_align with a function returning a pointer to a function
   should perhaps trigger a warning.  */
typedef void (F)(void);
ALIGN (1) F* fpF_i_1 (int);

ALIGN (SIZE_MAX) void*
fpvi_szmax (int);                 /* { dg-warning ".alloc_align. attribute argument value .\[0-9\]+. exceeds the number of function parameters 1" } */

ALIGN ("1") void*
fpvi_str_1 (int);                 /* { dg-warning ".alloc_align. attribute argument has type .char\\\[2]" } */

ALIGN ((void*)0) void*
fpvi_pv0 (int);                   /* { dg-warning ".alloc_align. attribute argument has type .void \\\*." } */

ALIGN ((double*)1) void*
fpvi_pd1 (int);                   /* { dg-warning ".alloc_align. attribute argument has type .double \\\*." } */

ALIGN (1) void*
fpvi_pv_1 (void*);                /* { dg-warning ".alloc_align. attribute argument value .1. refers to parameter type .void \\\*." } */

struct S { int i; };
ALIGN (2) void*
fpvi_S_2 (int, struct S);         /* { dg-warning ".alloc_align. attribute argument value .2. refers to parameter type .struct S." } */

