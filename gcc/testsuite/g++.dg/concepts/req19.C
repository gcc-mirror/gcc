// { dg-options "-std=c++1z -fconcepts" }

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
