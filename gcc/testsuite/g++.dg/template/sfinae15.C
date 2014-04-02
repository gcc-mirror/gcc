// PR c++/40944
// { dg-do run { target c++11 } }

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
