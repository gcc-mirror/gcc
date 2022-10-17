// PR c++/66451
// { dg-do run }

#if __cplusplus > 201100L
#define THROWING noexcept(false)
#else
#define THROWING
#endif

extern "C" void abort();

int c;
struct A
{
  int a;

  A(int new_a) : a(new_a) { ++c; }
  A(const A&); // not defined
  ~A() THROWING
  {
    --c;
    if(a==4)
      throw a;
  }
};

struct B
{
  A a[2];
  ~B() { }
};

int sink;
int main()
{
  try {
    B b = {3,4};
  } catch(...) { }
  if (c != 0) abort();
}
