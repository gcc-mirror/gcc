typedef int V __attribute__ ((vector_size (2 * sizeof (int))));
V
foo (int t)
{
  if (t < 10)
    return (V){1, 1};
  V v = { };
  return v - foo(t - 1);
}
