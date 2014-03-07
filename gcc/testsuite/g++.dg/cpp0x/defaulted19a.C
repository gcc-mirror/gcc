// We allocate a cookie to help us run the destructor if it's non-trivial,
// even if it's deleted.
// { dg-do run { target c++11 } }

struct B { ~B() {} };
struct A
{
  B b;
  ~A() = delete;
};

void *p = 0;
void *operator new[](__SIZE_TYPE__ t)
{
  p = ::operator new (t);
  return p;
}

int main()
{
  A* ap = new A[5];
  return ap == p;
}
