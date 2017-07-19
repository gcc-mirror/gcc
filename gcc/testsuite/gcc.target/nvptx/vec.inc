TYPE __attribute__((noinline, noclone))
vector_load (TYPE *p)
{
  return *p;
}

void __attribute__((noinline, noclone))
vector_store (TYPE *p, TYPE val)
{
  *p = val;
}

TYPE __attribute__((noinline, noclone))
vector_const ()
{
  TYPE res = {1, 2};
  return res;
}
