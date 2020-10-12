// PR c++/79504
// { dg-do compile { target c++11 } }

struct A
{
  void f () & {}

  template <typename ...Args>
    auto f (Args &&... args) && -> decltype (this->f (args...))
    {
      return this->f (args...);
    }
};

int main (){
  A p;
  p.f ();
  A{}.f();
}
