/* Tests the warnings for insufficient allocation size.  */
/* { dg-do compile } */
/* { dg-options "-Walloc-size -Wno-calloc-transposed-args" } */

struct S { int x[10]; };
void bar (struct S *);
typedef __SIZE_TYPE__ size_t;
void *myfree (void *, int, int);
void *mymalloc (int, int, size_t) __attribute__((malloc, malloc (myfree), alloc_size (3)));
void *mycalloc (int, int, size_t, size_t) __attribute__((malloc, malloc (myfree), alloc_size (3, 4)));

void
foo (void)
{
  struct S *p = (struct S *) __builtin_malloc (sizeof *p);
  __builtin_free (p);
  p = (struct S *) __builtin_malloc (sizeof p);		/* { dg-warning "allocation of insufficient size" } */
  __builtin_free (p);
  p = (struct S *) __builtin_alloca (sizeof p);		/* { dg-warning "allocation of insufficient size" } */
  bar (p);
  p = (struct S *) __builtin_calloc (1, sizeof p);	/* { dg-warning "allocation of insufficient size" } */
  __builtin_free (p);
  bar ((struct S *) __builtin_malloc (4));		/* { dg-warning "allocation of insufficient size" } */
  __builtin_free (p);
  p = (struct S *) __builtin_calloc (sizeof *p, 1);
  __builtin_free (p);
  p = __builtin_calloc (sizeof *p, 1);
  __builtin_free (p);
}

void
baz (void)
{
  struct S *p = (struct S *) mymalloc (42, 42, sizeof *p);
  myfree (p, 42, 42);
  p = (struct S *) mymalloc (42, 42, sizeof p);		/* { dg-warning "allocation of insufficient size" } */
  myfree (p, 42, 42);
  p = (struct S *) mycalloc (42, 42, 1, sizeof p);	/* { dg-warning "allocation of insufficient size" } */
  myfree (p, 42, 42);
  bar ((struct S *) mymalloc (42, 42, 4));		/* { dg-warning "allocation of insufficient size" } */
  myfree (p, 42, 42);
  p = (struct S *) mycalloc (42, 42, sizeof *p, 1);
  myfree (p, 42, 42);
  p = mycalloc (42, 42, sizeof *p, 1);
  myfree (p, 42, 42);
  p = mymalloc (42, 42, sizeof *p);
  myfree (p, 42, 42);
  p = mymalloc (42, 42, sizeof p);			/* { dg-warning "allocation of insufficient size" } */
  myfree (p, 42, 42);
  p = mycalloc (42, 42, 1, sizeof p);			/* { dg-warning "allocation of insufficient size" } */
  myfree (p, 42, 42);
  bar (mymalloc (42, 42, 4));				/* { dg-warning "allocation of insufficient size" } */
  myfree (p, 42, 42);
}
