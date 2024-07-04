typedef int __attribute__((__vector_size__ (32))) V;

void
foo (char *out)
{
  V v = (V) { };
  __builtin_strncpy (out, (char *)&v, 5);
}
