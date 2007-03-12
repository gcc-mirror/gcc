// PR c++/30328
// { dg-do link }
// { dg-options "" }

struct S
{
  signed int a:17;
} x;

typedef typeof (x.a) foo;

template <class T>
T* inc(T* p) { return p+1; }

int main ()
{
  foo x[2] = { 1,2 };
  int y[2] = { 1,2 };
  *inc(x);
  *inc(y);
  return 0;
}
