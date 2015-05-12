// PR c++/65133
// { dg-do compile { target c++11 } }

template<bool, typename Tp = void>
struct enable_if { };

template<typename Tp>
struct enable_if<true, Tp> { typedef Tp type; };

template <int I>
struct count
{
  using type = typename count<I-1>::type;
};

template <>
struct count<0>
{
  using type = void;
};

template <int I>
auto foo(typename enable_if<(I>=0)>::type *
	 = nullptr) -> typename count<I>::type { }

template <int I>
void foo(typename enable_if<(I<0)>::type * = nullptr) { }

int main()
{
  foo<2>();
  foo<-1>();
}
