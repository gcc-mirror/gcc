/* PR c/78284 - warn on malloc with very large arguments
   Test exercising the ability of the built-in allocation functions
   to detect and diagnose, without optimization, calls that attemnpt
   to allocate objects in excess of the number of bytes specified by
   -Walloc-larger-than=maximum.  */
/* { dg-do compile } */
/* { dg-require-effective-target alloca } */
/* { dg-options "-O0 -Wall -Walloc-size-larger-than=12345 -Wno-use-after-free" } */

#define MAXOBJSZ  12345

typedef __SIZE_TYPE__ size_t;

void sink (void*);


void test_lit (char *p, char *q)
{
  sink (__builtin_aligned_alloc (1, MAXOBJSZ));
  sink (__builtin_aligned_alloc (1, MAXOBJSZ + 1));   /* { dg-warning "argument 2 value .12346\[lu\]*. exceeds maximum object size 12345" } */

  sink (__builtin_alloca (MAXOBJSZ));
  sink (__builtin_alloca (MAXOBJSZ + 2));   /* { dg-warning "argument 1 value .12347\[lu\]*. exceeds maximum object size 12345" } */

  sink (__builtin_calloc (MAXOBJSZ, 1));
  sink (__builtin_calloc (1, MAXOBJSZ));

  /* Verify that the signed to unsigned conversion below doesn't cause
     a warning.  */
  sink (__builtin_calloc (p - q, 1));
  sink (__builtin_calloc (1, q - p));
  sink (__builtin_calloc (p - q, MAXOBJSZ));
  sink (__builtin_calloc (MAXOBJSZ, q - p));

  sink (__builtin_calloc (MAXOBJSZ / 2, 3));   /* { dg-warning "product .6172\[lu\]* \\* 3\[lu\]*. of arguments 1 and 2 exceeds maximum object size 12345" } */
  sink (__builtin_calloc (4, MAXOBJSZ / 3));   /* { dg-warning "product .4\[lu\]* \\* 4115\[lu\]*. of arguments 1 and 2 exceeds maximum object size 12345" } */

  sink (__builtin_malloc (MAXOBJSZ));
  sink (__builtin_malloc (MAXOBJSZ + 3));   /* { dg-warning "argument 1 value .12348\[lu\]*. exceeds maximum object size 12345" } */

  sink (__builtin_realloc (p, MAXOBJSZ));
  sink (__builtin_realloc (p, MAXOBJSZ + 4));  /* { dg-warning "argument 2 value .12349\[lu\]*. exceeds maximum object size 12345" } */
}


enum { max = MAXOBJSZ };

void test_cst (char *p, char *q)
{
  sink (__builtin_aligned_alloc (1, max));
  sink (__builtin_aligned_alloc (1, max + 1));   /* { dg-warning "argument 2 value .12346\[lu\]*. exceeds maximum object size 12345" } */

  sink (__builtin_alloca (max));
  sink (__builtin_alloca (max + 2));   /* { dg-warning "argument 1 value .12347\[lu\]*. exceeds maximum object size 12345" } */

  sink (__builtin_calloc (max, 1));
  sink (__builtin_calloc (1, max));

  /* Verify that the signed to unsigned conversion below doesn't cause
     a warning.  */
  sink (__builtin_calloc (p - q, 1));
  sink (__builtin_calloc (1, q - p));
  sink (__builtin_calloc (p - q, max));
  sink (__builtin_calloc (max, q - p));

  sink (__builtin_calloc (max / 2, 3));   /* { dg-warning "product .6172\[lu\]* \\* 3\[lu\]*. of arguments 1 and 2 exceeds maximum object size 12345" } */
  sink (__builtin_calloc (4, max / 3));   /* { dg-warning "product .4\[lu\]* \\* 4115\[lu\]*. of arguments 1 and 2 exceeds maximum object size 12345" } */

  sink (__builtin_malloc (max));
  sink (__builtin_malloc (max + 3));   /* { dg-warning "argument 1 value .12348\[lu\]*. exceeds maximum object size 12345" } */

  sink (__builtin_realloc (p, max));
  sink (__builtin_realloc (p, max + 4));  /* { dg-warning "argument 2 value .12349\[lu\]*. exceeds maximum object size 12345" } */
}
