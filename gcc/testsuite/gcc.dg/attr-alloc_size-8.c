/* PR c/78284 - warn on malloc with very large arguments
   Test to exercise the interaction of the -Walloca-larger-than,
   -Wvla-larger-than, and -Walloc-size-larger-than options.  The former
   two more specific options override the more general latter option.  */
/* { dg-do compile } */
/* { dg-require-effective-target alloca } */
/* { dg-options "-O2 -Walloc-size-larger-than=123 -Walloca-larger-than=234 -Wvla-larger-than=345" } */

typedef __SIZE_TYPE__ size_t;

void sink (void*);

static size_t alloc_size_limit (void)
{
  return 123;
}

static size_t alloca_limit (void)
{
  return 234;
}

static size_t vla_limit (void)
{
  return 345;
}

void test_alloca (void)
{
  void *p;

  /* No warning should be issued for the following call because the more
     permissive alloca limit overrides the stricter alloc_size limit.  */
  p = __builtin_alloca (alloca_limit ());
  sink (p);

  p = __builtin_alloca (alloca_limit () + 1);   /* { dg-warning "argument to .alloca. is too large" } */
  sink (p);
}

void test_vla (void)
{
  /* Same as above, no warning should be issued here because the more
     permissive VLA limit overrides the stricter alloc_size limit.  */
  char vla1 [vla_limit ()];
  sink (vla1);

  char vla2 [vla_limit () + 1];   /* { dg-warning "argument to variable-length array is too large" } */
  sink (vla2);
}

void test_malloc (void)
{
  void *p;
  p = __builtin_malloc (alloc_size_limit ());
  sink (p);

  p = __builtin_malloc (alloc_size_limit () + 1);   /* { dg-warning "argument 1 value .124\[lu\]*. exceeds maximum object size 123" } */
  sink (p);
}
