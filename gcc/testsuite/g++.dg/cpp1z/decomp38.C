// { dg-do compile { target c++17 } }

class X
{
   int a, b;
   void f()
   {
     auto[x,y] = *this;
   }
};

class X2
{
   int a, b;
   void f(X2& other)
   {
     auto[x,y] = other;
   }
};

struct X3
{
  friend void foo();
private:
  int a;
};

void foo()
{
  X3 x;
  auto [a] = x;
}

struct X4
{
  int a;
};

struct X5 : private X4
{
  friend void foo2();
};

void foo2() {
  X5 x;
  auto [a] = x;
}
