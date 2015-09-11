// PR c++/65054

const char *
foo (void)
{
  return ((char *const) "abc" + 1);
}
