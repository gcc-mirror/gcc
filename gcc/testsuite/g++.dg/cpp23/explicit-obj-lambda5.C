// P0847R7
// { dg-do run { target c++23 } }

// calls to (captureless) lambda with explicit object parameter of unrelated type
// with an appropriate converting constructor

inline constexpr int magic = 42;

struct S {
  int _v;  
  template<typename T>
  S(T) : _v(magic) {}
};

int main()
{
  auto f = [](this S self){ return self._v; };
  if (f () != magic)
    __builtin_abort ();
}

