// PR c++/89622
// { dg-do compile { target c++11 } }
// { dg-options "-Wno-system-headers -w" }
// { dg-bogus "says that these are ambiguous" "" { target *-*-* } 0 }
// { dg-bogus "candidate 1" "" { target *-*-* } 0 }
// { dg-bogus "candidate 2" "" { target *-*-* } 0 }

# 3 "pr89622.h" 3
template<typename T>
struct X
{
  X() { }
  template<typename U> X(int, U&&) { }
  template<typename U> X(char, const X<U>&) { }
};

template<typename T>
X<T> wrap_X(X<T> x)
{
  return X<T>('a', x);
}

int main()
{
  X<void> x;
  wrap_X(x);
}
