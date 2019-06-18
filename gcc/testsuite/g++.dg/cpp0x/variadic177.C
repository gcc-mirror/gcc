// PR c++/71548
// { dg-do compile { target c++11 } }

template<typename> class fl {};
template<typename = void, template<class...> class = fl>
struct S {};
template<typename... T>
void f(S<T...> ) {}
void lol() {
    S<> s;
    f(s); // { dg-error "no matching function for call to" }
}
