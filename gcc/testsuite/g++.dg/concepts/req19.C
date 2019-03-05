// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

struct B
{
  template <class T> void f(T t)
    requires requires (T tt) { tt; }
  { }
};

int main()
{
  B().f(42);
}
