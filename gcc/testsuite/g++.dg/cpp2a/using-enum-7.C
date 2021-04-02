// PR c++/97874
// { dg-do compile { target c++20 } }

struct A { enum E { kl }; };

template <typename UQ>
int
v4 ()
{
  using UQ::kl;
  return kl;
}

template <typename UQ>
int
v5 ()
{
  using UQ::kl;			// { dg-error "not a class" }
  return kl;			// { dg-error "not declared" }
}

int main()
{
  v4<A>();
  v4<A::E>();
  v5<int>();
}
