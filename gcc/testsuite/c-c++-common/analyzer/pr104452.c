/* { dg-additional-options "-O" } */

void
test_1 (void)
{
  int __attribute__((__vector_size__ (16))) x;
  for (unsigned i = 0; i < 4;)
    if (x[i]) /* { dg-warning "use of uninitialized value" } */
      __builtin_abort ();
}
