typedef unsigned char __attribute__((__vector_size__ (1))) U;

U
foo (U u)
{
  u = u == u;
  return (~(u >> 255));
}
