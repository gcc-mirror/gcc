/* Test that __builtin_prefetch does no harm.

   Prefetch using some invalid rw and locality values.  These must be
   compile-time constants.  */

/* { dg-do run } */

extern void exit (int);

enum locality { none, low, moderate, high, bogus };
enum rw { read, write };

int arr[10];

void
good (int *p)
{
  __builtin_prefetch (p, 0, 0);
  __builtin_prefetch (p, 0, 1);
  __builtin_prefetch (p, 0, 2);
  __builtin_prefetch (p, 0, 3);
  __builtin_prefetch (p, 1, 0);
  __builtin_prefetch (p, 1, 1);
  __builtin_prefetch (p, 1, 2);
  __builtin_prefetch (p, 1, 3);
}

void
bad (int *p)
{
  __builtin_prefetch (p, -1, 0);  /* { dg-warning "invalid second argument to '__builtin_prefetch'; using zero" } */
  __builtin_prefetch (p, 2, 0);   /* { dg-warning "invalid second argument to '__builtin_prefetch'; using zero" } */
  __builtin_prefetch (p, bogus, 0);   /* { dg-warning "invalid second argument to '__builtin_prefetch'; using zero" } */
  __builtin_prefetch (p, 0, -1);  /* { dg-warning "invalid third argument to '__builtin_prefetch'; using zero" } */
  __builtin_prefetch (p, 0, 4);   /* { dg-warning "invalid third argument to '__builtin_prefetch'; using zero" } */
  __builtin_prefetch (p, 0, bogus);   /* { dg-warning "invalid third argument to '__builtin_prefetch'; using zero" } */
}

int
main ()
{
  good (arr);
  bad (arr);
  exit (0);
}
