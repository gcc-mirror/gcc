
static int next_buffer = 0;
void bar (void);

static int t = 1, u = 0;

long
foo (unsigned int offset)
{
  unsigned i, buffer;
  int x;
  char *data;

  i = u;
  if (i)
    return i * 0xce2f;

  buffer = next_buffer;
  data = (char *) (buffer * 0xce2f);
  for (i = 0; i < 2; i++)
    bar ();
  buffer = next_buffer;
  return buffer * 0xce2f + offset;

}

void
bar (void)
{
}

int
main ()
{
  if (foo (3) != 3)
    __builtin_abort ();
  next_buffer = 1;
  if (foo (2) != 0xce2f + 2)
    __builtin_abort ();
  __builtin_exit (0);
}
