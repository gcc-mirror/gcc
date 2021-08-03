int test (void)
{
  int *ptr = (int *)__builtin_malloc (sizeof (int));
  *ptr = 42; /* { dg-warning "dereference of possibly-NULL 'ptr'" } */
  __builtin_free (ptr);

  return *ptr; /* { dg-warning "use after 'free' of 'ptr'" "use-after-free" } */
}
