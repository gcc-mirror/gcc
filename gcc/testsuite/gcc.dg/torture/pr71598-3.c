/* { dg-do run } */

enum e1 { A, B };
enum e2 { C, D };

__attribute__((noinline,noclone))
enum e1 f(unsigned int *p)
{
  *(enum e1 *)p = A;
  *(enum e2 *)p = D;
  return *(enum e1 *)p;
}

int main()
{
  unsigned int storage;

  if (f(&storage) != B)
    __builtin_abort();
  return 0;
}
