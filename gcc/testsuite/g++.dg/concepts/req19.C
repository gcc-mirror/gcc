// { dg-options "-std=c++17 -fconcepts" }

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
