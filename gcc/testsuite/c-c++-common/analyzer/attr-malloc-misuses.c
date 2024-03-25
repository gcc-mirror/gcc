extern void free (void *);

int not_a_fn __attribute__ ((malloc (free))); /* { dg-warning "'malloc' attribute ignored; valid only for functions" } */

void void_return (void) __attribute__ ((malloc(free))); /* { dg-warning "'malloc' attribute ignored on functions returning 'void'" } */

void test_void_return (void)
{
  void_return ();
}

extern void void_args (void); /* { dg-message "declared here" } */
void *has_malloc_with_void_args (void)
  __attribute__ ((malloc(void_args))); /* { dg-error "'malloc' attribute argument 1 must take a pointer type as its first argument; have 'void'" } */

extern void no_args (); /* { dg-message "declared here" } */
void *has_malloc_with_no_args (void)
  __attribute__ ((malloc(no_args))); /* { dg-error "'malloc' attribute argument 1 must take a pointer type as its first argument" } */
