// { dg-do compile { target concepts } }

class A
{
  static void f(int);
public:
  template <class T> void g(T t)
    requires requires { f(t); }
  {}
};

int main()
{
  A().g(42);
}
