struct S
{
  unsigned i:4;
  unsigned :0;
} s;
static void *f(void)
{
  return &s;
}
