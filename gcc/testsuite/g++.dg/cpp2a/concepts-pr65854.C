// PR c++/65854
// { dg-do compile { target c++2a } }

// Handle alias templates in type requirements.

template<typename T1, typename T2>
struct BTT { };

template<typename T>
struct BTT<T,T> { using type = int; };

template<typename T1, typename T2>
using Alias1 = typename BTT<T1, T2>::type;

template<typename T1, typename T2>
concept C = requires() { typename Alias1<T1, T2>; }; // { dg-message "in requirements" }

template<typename T1, typename T2>
  requires C<T1, T2>
int f();

auto i = f<char, int>(); // { dg-error "" }
