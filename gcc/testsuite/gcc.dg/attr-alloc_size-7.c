/* PR c/78284 - warn on malloc with very large arguments
   Test exercising the ability of the built-in allocation functions to
   detect and diagnose calls that attemnpt to allocate objects in excess
   of the maximum specified by -Walloc-size-larger-than=maximum.  */
/* { dg-do compile } */
/* { dg-require-effective-target alloca } */
/* { dg-options "-O1 -Wall -Walloc-size-larger-than=12345" } */

#define SIZE_MAX   __SIZE_MAX__
#define MAXOBJSZ   12345

typedef __SIZE_TYPE__ size_t;

void sink (void*);

#pragma GCC push_options
/* Verify that constant evaluation takes place even at -O0.  */
#pragma GCC optimize ("0")

void test_cst (void *p)
{
  enum { max = MAXOBJSZ };

  sink (__builtin_aligned_alloc (1, max));
  sink (__builtin_aligned_alloc (1, max + 1));   /* { dg-warning "argument 2 value .12346\[lu\]*. exceeds maximum object size 12345" } */

  sink (__builtin_alloca (max));
  sink (__builtin_alloca (max + 2));   /* { dg-warning "argument 1 value .12347\[lu\]*. exceeds maximum object size 12345" } */

  sink (__builtin_calloc (1, max));
  sink (__builtin_calloc (max, 1));

  sink (__builtin_calloc (max / 2, 3));   /* { dg-warning "product .6172\[lu\]* \\* 3\[lu\]*. of arguments 1 and 2 exceeds maximum object size 12345" } */
  sink (__builtin_calloc (4, max / 3));   /* { dg-warning "product .4\[lu\]* \\* 4115\[lu\]*. of arguments 1 and 2 exceeds maximum object size 12345" } */

  sink (__builtin_malloc (max));
  sink (__builtin_malloc (max + 3));   /* { dg-warning "argument 1 value .12348\[lu\]*. exceeds maximum object size 12345" } */

  sink (__builtin_realloc (p, max));
  sink (__builtin_realloc (p, max + 4));  /* { dg-warning "argument 2 value .12349\[lu\]*. exceeds maximum object size 12345" } */
}


/* Variable evaluation needs -O1.  */
#pragma GCC pop_options

__attribute__ ((noipa)) void test_var (void *p)
{
  size_t max = MAXOBJSZ;

  sink (__builtin_aligned_alloc (1, max));
  sink (__builtin_aligned_alloc (1, max + 1));   /* { dg-warning "argument 2 value .12346\[lu\]*. exceeds maximum object size 12345" } */

  sink (__builtin_alloca (max));
  sink (__builtin_alloca (max + 2));   /* { dg-warning "argument 1 value .12347\[lu\]*. exceeds maximum object size 12345" } */

  sink (__builtin_calloc (1, max));
  sink (__builtin_calloc (max, 1));

  sink (__builtin_calloc (max / 2, 3));   /* { dg-warning "product .6172\[lu\]* \\* 3\[lu\]*. of arguments 1 and 2 exceeds maximum object size 12345" } */
  sink (__builtin_calloc (4, max / 3));   /* { dg-warning "product .4\[lu\]* \\* 4115\[lu\]*. of arguments 1 and 2 exceeds maximum object size 12345" } */

  sink (__builtin_malloc (max));
  sink (__builtin_malloc (max + 3));   /* { dg-warning "argument 1 value .12348\[lu\]*. exceeds maximum object size 12345" } */

  sink (__builtin_realloc (p, max));
  sink (__builtin_realloc (p, max + 4));  /* { dg-warning "argument 2 value .12349\[lu\]*. exceeds maximum object size 12345" } */
}


/* Value range evaluation (apparently) needs -O2 here.  */
#pragma GCC optimize ("2")

static size_t maxobjsize (void)
{
  return MAXOBJSZ;
}

__attribute__ ((noipa)) void test_range (void *p, size_t range)
{
  /* Make sure the variable is at least as large as the maximum object
     size but also make sure that it's guaranteed not to be too big to
     increment (and wrap around).  */
  size_t max = maxobjsize ();

  if (range < max || 2 * max <= range)
    range = maxobjsize ();

  sink (__builtin_aligned_alloc (1, range));
  sink (__builtin_aligned_alloc (1, range + 1));   /* { dg-warning "argument 2 range \\\[12346\[lu\]*, \[0-9\]+\[lu\]*\\\] exceeds maximum object size 12345" } */

  sink (__builtin_alloca (range));
  sink (__builtin_alloca (range + 2));   /* { dg-warning "argument 1 range \\\[12347\[lu\]*, \[0-9\]+\[lu\]*\\\] exceeds maximum object size 12345" } */

  sink (__builtin_calloc (range, 1));
  sink (__builtin_calloc (1, range));

  sink (__builtin_calloc (range / 2, 3));   /* { dg-warning "product .6172\[lu\]* \\* 3\[lu\]*. of arguments 1 and 2 exceeds maximum object size 12345" } */
  sink (__builtin_calloc (4, range / 3));   /* { dg-warning "product .4\[lu\]* \\* 4115\[lu\]*. of arguments 1 and 2 exceeds maximum object size 12345" } */

  sink (__builtin_malloc (range));
  sink (__builtin_malloc (range + 3));   /* { dg-warning "argument 1 range \\\[12348\[lu\]*, 24692\[lu\]*\\\] exceeds maximum object size 12345" } */

  sink (__builtin_realloc (p, range));
  sink (__builtin_realloc (p, range + 4));  /* { dg-warning "argument 2 range \\\[12349\[lu\]*, 24693\[lu\]*\\\] exceeds maximum object size 12345" } */
}
