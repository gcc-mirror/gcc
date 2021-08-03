typedef int V __attribute__ ((vector_size (2 * sizeof (int))));
V
foo (void)
{
  V v = { };
  return v - foo();
}
