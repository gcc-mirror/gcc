// PR c++/50863
// { dg-options -std=gnu++0x }

struct T {
  template<typename F>
  T(F) { }
};

int main()
{
  T t{ []{ } };
}
