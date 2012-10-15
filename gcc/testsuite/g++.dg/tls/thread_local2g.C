// { dg-do run }
// { dg-options "-std=c++11" }
// { dg-require-effective-target tls_runtime }
// { dg-require-alias }

extern "C" void abort();

struct A
{
  A();
  int i;
};

thread_local A a;

A &f()
{
  return a;
}

int j;
A::A(): i(j) { }

int main()
{
  j = 42;
  if (f().i != 42)
    abort ();
}
