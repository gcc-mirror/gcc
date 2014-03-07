// PR c++/50863
// { dg-do compile { target c++11 } }

struct T {
  template<typename F>
  T(F) { }
};

int main()
{
  T t{ []{ } };
}
