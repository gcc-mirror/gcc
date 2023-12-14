int __attribute__((__vector_size__ (4))) v;

void
foo (void)
{
  v | v << 1;
}
