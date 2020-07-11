// { dg-do compile { target c++20 } }

struct A {
  int i;
  // auto operator<=> (const A&) = default;
};
template <typename T, T t> void f()
{
  g(t);				// { dg-error "not declared" }
}

int main()
{
  f<A,A{1}>();
}

// { dg-message "T t = A{1}" "" { target *-*-* } 0 }
