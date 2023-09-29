void *realloc (void *, unsigned long)
  __attribute__((__nothrow__, __leaf__))
  __attribute__((__warn_unused_result__)) __attribute__((__alloc_size__ (2)));

long *
slurp (long *buffer, unsigned long file_size)
{
  unsigned long cc;
  if (!__builtin_add_overflow (file_size - file_size % sizeof (long),
			       2 * sizeof (long), &cc))
    buffer = (long *) realloc (buffer, cc);
  return buffer;
}

long *
slurp1 (long *buffer, unsigned long file_size)
{
  return (long *) realloc (buffer, file_size - file_size % sizeof (long));
}

long *
slurp2 (long *buffer, unsigned long file_size)
{
  return (long *) realloc (buffer, (file_size / sizeof (long)) * sizeof (long));
}
