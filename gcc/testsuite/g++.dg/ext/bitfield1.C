// PR c++/30328
// { dg-do link }
// { dg-options "" }

typedef int int32_t __attribute__((mode (__SI__)));

struct S
{
  int32_t a:17;
} x;

typedef typeof (x.a) foo;

template <class T>
T* inc(T* p) { return p+1; }

int main ()
{
  foo x[2] = { 1,2 };
  int32_t y[2] = { 1,2 };
  *inc(x);
  *inc(y);
  return 0;
}
