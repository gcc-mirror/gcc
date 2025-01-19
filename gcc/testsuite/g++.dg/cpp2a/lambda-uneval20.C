// PR c++/118391
// { dg-do compile { target c++20 } }

template<typename>
using A = int;

template<typename T>
using B = decltype([]<typename> {}.template operator()<T>());

template<typename T>
using C = A<B<T>>;

C<int> x;

int main() {}
