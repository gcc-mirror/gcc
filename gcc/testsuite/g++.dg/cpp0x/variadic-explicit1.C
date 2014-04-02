// { dg-do compile { target c++11 } }

template<class T, class U> struct A { };
template<class... T, class ... U> void f( A<T,U>... p);

void g() {
    f<int>(
        A<int,unsigned>(),
        A<short,unsigned short>()
        );
}
