// { dg-do run { target c++17 } }

int x[10];
int y[10];
bool c() { return true; }

void f(int i, int v)
{
  (c() ? x : y)[i] = v;
}

void g(int i, int v)
{
  i[c() ? x : y] = v;
}

int main()
{
  f(0, 1);
  if (x[0] != 1)
    __builtin_abort();

  g(0, 2);
  if (x[0] != 2)
    __builtin_abort();
}
