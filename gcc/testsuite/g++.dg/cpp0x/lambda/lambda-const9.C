// PR c++/86147
// { dg-do compile { target c++11 } }

template <class T, T N> struct X { };

struct A { static constexpr int value = 0; };

template<class C>
void foo() {
    constexpr int N = C::value;
    auto f = [&]{  X<int, N> a; };
}

int main() { 
    foo<A>();
    return 0;
}
