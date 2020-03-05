// PR c++/85552
// { dg-do compile { target c++11 } }

template<typename T>
struct uptr {
  uptr() { }
  uptr(void*) { }
  ~uptr() { static_assert(sizeof(T), "complete type"); }
};

class S;

class Compiles
{
  uptr<S> s;
};

class DoesntCompile
{
  ~DoesntCompile();
  DoesntCompile();

  uptr<S> s1 { };
  uptr<S> s2 { nullptr };
};

int main()
{
  return 0;
}
