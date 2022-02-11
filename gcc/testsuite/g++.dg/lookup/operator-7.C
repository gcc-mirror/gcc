// PR c++/100465

namespace N
{
  struct string
  {
    template<typename T>
    void operator+=(T);
  };

  struct A {
    void operator+=(char); // #1

    template<typename T>
    void f() {
      string s;
      s += T();
    }

    void g() {
      f<char>();
    }
  };
} // namespace N

template<typename T>
void operator+=(N::string, T);
