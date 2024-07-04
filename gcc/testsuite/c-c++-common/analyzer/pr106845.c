int buf_size;

int
main (void)
{
  char buf[buf_size];

  __builtin_memset (&buf[1], 0, buf_size);

  return 0;
}
