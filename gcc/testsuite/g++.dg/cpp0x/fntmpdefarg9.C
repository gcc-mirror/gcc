// PR c++/85866
// { dg-do compile { target c++11 } }

template<typename _Tp, typename _Up = _Tp&&>
_Up
__declval(int);

template<typename _Tp>
_Tp
__declval(long);

template<typename _Tp>
auto declval() noexcept -> decltype(__declval<_Tp>(0));

template<typename...>
using void_t = void;

template<typename U, typename V,
	 void_t<decltype ( (declval<U>().*declval<V>()) () )
		>* = nullptr>
void boom(){}

struct Foo {
  void bar(){}
};

int main() {
  boom<Foo, decltype(&Foo::bar)>();
}
