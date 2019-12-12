/* { dg-do run } */

typedef long v2di __attribute__((vector_size(16)));
v2di v;
void __attribute__((noinline))
foo()
{
  v = (v2di){v[1], v[0]};
}

int main()
{
  v[0] = 1;
  foo ();
  if (v[0] != 0 || v[1] != 1)
    __builtin_abort ();
  return 0;
}
