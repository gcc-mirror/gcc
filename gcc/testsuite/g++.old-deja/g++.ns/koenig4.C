void f();
void f(int);

namespace A{
  struct S{
    void f();
    void f(S);
  };
  void f(S&){}
  void h(S&){}
}

template<class T>
void g(T t){
  f(t);
}

int main()
{
  A::S s;
  f(s);
  g(s);
  h(s);
}
