// PR c++/71350
// { dg-do compile { target c++11 } }

template<typename T, unsigned int N>
struct Array
{
    T data[N];
};

template<typename T>
struct Foo
{
    int operator[](const Array<int, 2>& i) const { return 0; }
    auto bar() -> decltype((*this)[{1,2}] * 0) {
      return *this;		// { dg-error "cannot convert" }
    }
};

template struct Foo<int>;
