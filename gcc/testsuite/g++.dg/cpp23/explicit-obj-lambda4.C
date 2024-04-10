// P0847R7
// { dg-do run { target c++23 } }

// calls to call operator of a lambda with captures with an implicit object argument
// that derives from the lambda closure object

template<typename T>
struct S : T {
    using T::operator();
};

template<typename T>
S(T) -> S<T>; 

int main()
{
  static constexpr int magic = 42;
  int n = magic;
  S s{[n](this auto&&){return n;}};
  if (s () != magic)
    __builtin_abort ();
}

