// { dg-do run { target c++11 } }
// { dg-add-options tls }
// { dg-require-effective-target tls_runtime }

extern "C" void abort();

struct A
{
  A();
  int i;
};

A &f()
{
  thread_local A a;
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
