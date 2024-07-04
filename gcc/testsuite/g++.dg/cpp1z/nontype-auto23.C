// PR c++/99186
// { dg-do compile { target c++17 } }

template<int N, class T, class... U>
struct tuple_impl : tuple_impl<N + 1, U...> { };

template<int N, class T>
struct tuple_impl<N, T> { };

template<class T, class U>
struct tuple : tuple_impl<0, T, U> { };

template<class T, int N, class... U>
void get(const tuple_impl<N, T, U...>&);

template<auto> struct S;

int main() {
   tuple<S<1>,S<1U>> x;
   get<S<1U>>(x);
   get<S<1>>(x);
}
