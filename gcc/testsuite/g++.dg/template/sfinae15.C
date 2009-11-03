// PR c++/40944
// { dg-options -std=c++0x }
// { dg-do run }

template<typename T>
struct make { static T&& it(); };

void (*pf)(int&) = 0;

template< typename T >
int bar(T const& x,
        decltype( pf(make<T const&>::it()) )* = 0 // SFINAE!
        ) {
    return 1;
}

int bar(...) {
    return 0;
}

int main() {
    return bar(42);
}
