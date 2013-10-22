// { dg-options "-std=c++11" }
// { dg-do run }

extern "C" void abort();

void f(int i) { if (i != 42) abort(); }

int main()
{
  f({42});
  return {0};
}
