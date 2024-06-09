// Tests the warnings for insufficient allocation size.
// { dg-do compile }
// { dg-options "-Walloc-size -Wno-calloc-transposed-args" }

struct S { int x[10]; };
void bar (S *);
typedef __SIZE_TYPE__ size_t;
void *myfree (void *, int, int);
void *mymalloc (int, int, size_t) __attribute__((malloc, malloc (myfree), alloc_size (3)));
void *mycalloc (int, int, size_t, size_t) __attribute__((malloc, malloc (myfree), alloc_size (3, 4)));

void
foo (void)
{
  S *p = (S *) __builtin_malloc (sizeof *p);
  __builtin_free (p);
  p = (S *) __builtin_malloc (sizeof p);	// { dg-warning "allocation of insufficient size" }
  __builtin_free (p);
  p = (S *) __builtin_alloca (sizeof p);	// { dg-warning "allocation of insufficient size" }
  bar (p);
  p = (S *) __builtin_calloc (1, sizeof p);	// { dg-warning "allocation of insufficient size" }
  __builtin_free (p);
  bar ((S *) __builtin_malloc (4));		// { dg-warning "allocation of insufficient size" }
  __builtin_free (p);
  p = (S *) __builtin_calloc (sizeof *p, 1);
  __builtin_free (p);
}

void
baz (void)
{
  S *p = (S *) mymalloc (42, 42, sizeof *p);
  myfree (p, 42, 42);
  p = (S *) mymalloc (42, 42, sizeof p);	// { dg-warning "allocation of insufficient size" }
  myfree (p, 42, 42);
  p = (S *) mycalloc (42, 42, 1, sizeof p);	// { dg-warning "allocation of insufficient size" }
  myfree (p, 42, 42);
  bar ((S *) mymalloc (42, 42, 4));		// { dg-warning "allocation of insufficient size" }
  myfree (p, 42, 42);
  p = (S *) mycalloc (42, 42, sizeof *p, 1);
  myfree (p, 42, 42);
  p = static_cast <S *> (mycalloc (42, 42, sizeof *p, 1));
  myfree (p, 42, 42);
  p = static_cast <S *> (mymalloc (42, 42, sizeof *p));
  myfree (p, 42, 42);
  p = static_cast <S *> (mymalloc (42, 42, sizeof p));		// { dg-warning "allocation of insufficient size" }
  myfree (p, 42, 42);
  p = static_cast <S *> (mycalloc (42, 42, 1, sizeof p));	// { dg-warning "allocation of insufficient size" }
  myfree (p, 42, 42);
  bar (static_cast <S *> (mymalloc (42, 42, 4)));		// { dg-warning "allocation of insufficient size" }
  myfree (p, 42, 42);
}
