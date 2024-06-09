/* { dg-do compile } */
/* { dg-options "-Wcalloc-transposed-args" } */

typedef __SIZE_TYPE__ size_t;
void free (void *);
void *calloc (size_t, size_t);
void *myfree (void *, int, int);
void *mycalloc (int, int, size_t, size_t) __attribute__((malloc, malloc (myfree), alloc_size (3, 4)));

void
foo (int n)
{
  void *p;
  p = __builtin_calloc (1, sizeof (int));
  __builtin_free (p);
  p = __builtin_calloc (n, sizeof (int));
  __builtin_free (p);
  p = __builtin_calloc (sizeof (int), 1);		/* { dg-warning "'__builtin_calloc' sizes specified with 'sizeof' in the earlier argument and not in the later argument" } */
  __builtin_free (p);					/* { dg-message "earlier argument should specify number of elements, later size of each element" "" { target *-*-* } .-1 } */
  p = __builtin_calloc (sizeof (int), n);		/* { dg-warning "'__builtin_calloc' sizes specified with 'sizeof' in the earlier argument and not in the later argument" } */
  __builtin_free (p);					/* { dg-message "earlier argument should specify number of elements, later size of each element" "" { target *-*-* } .-1 } */
  p = __builtin_calloc ((sizeof (int)), 1);		/* { dg-warning "'__builtin_calloc' sizes specified with 'sizeof' in the earlier argument and not in the later argument" } */
  __builtin_free (p);					/* { dg-message "earlier argument should specify number of elements, later size of each element" "" { target *-*-* } .-1 } */
  p = __builtin_calloc (sizeof (int) + 0, 1);
  __builtin_free (p);
  p = __builtin_calloc (sizeof (int), sizeof (char));
  __builtin_free (p);
  p = __builtin_calloc (1 * sizeof (int), 1);
  __builtin_free (p);
  p = calloc (1, sizeof (int));
  free (p);
  p = calloc (n, sizeof (int));
  free (p);
  p = calloc (sizeof (int), 1);				/* { dg-warning "'calloc' sizes specified with 'sizeof' in the earlier argument and not in the later argument" } */
  free (p);						/* { dg-message "earlier argument should specify number of elements, later size of each element" "" { target *-*-* } .-1 } */
  p = calloc (sizeof (int), n);				/* { dg-warning "'calloc' sizes specified with 'sizeof' in the earlier argument and not in the later argument" } */
  free (p);						/* { dg-message "earlier argument should specify number of elements, later size of each element" "" { target *-*-* } .-1 } */
  p = calloc (sizeof (int), sizeof (char));
  free (p);
  p = calloc (1 * sizeof (int), 1);
  free (p);
  p = mycalloc (42, 42, 1, sizeof (int));
  myfree (p, 42, 42);
  p = mycalloc (42, 42, n, sizeof (int));
  myfree (p, 42, 42);
  p = mycalloc (42, 42, sizeof (int), 1);		/* { dg-warning "'mycalloc' sizes specified with 'sizeof' in the earlier argument and not in the later argument" } */
  myfree (p, 42, 42);					/* { dg-message "earlier argument should specify number of elements, later size of each element" "" { target *-*-* } .-1 } */
  p = mycalloc (42, 42, sizeof (int), n);		/* { dg-warning "'mycalloc' sizes specified with 'sizeof' in the earlier argument and not in the later argument" } */
  myfree (p, 42, 42);					/* { dg-message "earlier argument should specify number of elements, later size of each element" "" { target *-*-* } .-1 } */
  p = mycalloc (42, 42, sizeof (int), sizeof (char));
  myfree (p, 42, 42);
  p = mycalloc (42, 42, 1 * sizeof (int), 1);
  myfree (p, 42, 42);
}
