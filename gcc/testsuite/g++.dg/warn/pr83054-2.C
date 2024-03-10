// PR ipa/83054
// { dg-options "-O3 -Wsuggest-final-types" }
// { dg-do compile }

// A throwing dtor in C++98 mode changes the warning.
#if __cplusplus < 201100L
#define NOTHROW throw()
#else
#define NOTHROW noexcept
#endif

extern "C" int printf (const char *, ...);
struct foo // { dg-warning "final would enable devirtualization of 1 call" }
{
  static int count;
  void print (int i, int j) { printf ("foo[%d][%d] = %d\n", i, j, x); }
  int x;
  foo () {
    x = count++;
    printf("this %d = %x\n", x, (void *)this);
  }
  virtual ~foo () NOTHROW {
    printf("this %d = %x\n", x, (void *)this);
    --count;
  }
};
int foo::count;


int main ()
{
  foo *arr[9];
  for (int i = 0; i < 9; ++i)
    arr[i] = new foo();
  if (foo::count != 9)
    return 1;
  for (int i = 0; i < 9; ++i)
    arr[i]->print(i / 3, i % 3);
  for (int i = 0; i < 9; ++i)
    delete arr[i];
  if (foo::count != 0)
    return 1;
  return 0;
}
