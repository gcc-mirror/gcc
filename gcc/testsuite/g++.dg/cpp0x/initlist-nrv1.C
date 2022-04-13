// PR c++/101442
// { dg-do run { target c++11 } }

bool destroyed = false;

struct A
{
  A() {}
  A(const A &) = delete;
  A &operator=(const A &) = delete;
  ~A() {destroyed = true;}
};

struct B
{
  const A &a;
  struct string {
    string(const char*) { }
    ~string() { }
  } s;
};

B foo()
{
  B ret{ A{}, "" };
  return ret;
}

int main()
{
  B b = foo();
  if (!destroyed)
    __builtin_abort();
}
