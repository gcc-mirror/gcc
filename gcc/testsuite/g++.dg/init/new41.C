// PR c++/55446
// { dg-do run }

struct S
{
  S() { }
};

int n = 1;

void* operator new[](__SIZE_TYPE__)
{
  n = -1;
  return &n;
}

int main()
{
  new S[0];
  if (n != -1)
    __builtin_abort();
}
