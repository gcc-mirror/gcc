// { dg-do compile { target c++2a } }

template<typename ...>
constexpr bool r () { return true; }

template<typename ... Ts>
  requires r<Ts...>() // { dg-error "enclose" }
void f() { }

template<typename T, T N>
  requires ++N // { dg-error "enclose" }
void f() { }

template<typename T, T N>
  requires N++ // { dg-error "enclose" }
void f() { }

template<typename T, T N>
  requires N == 0 // { dg-error "enclose" }
void f() { }

template<typename T, T N>
  requires N ? true : false // { dg-error "enclose" }
void f() { }

template<typename T, T N>
  requires N = 0 // { dg-error "enclose" }
void f() { }

template<typename T, T N>
  requires N + 1 // { dg-error "enclose" }
void f() { }

template<typename T, T N>
  requires N - 1 // { dg-error "enclose" }
void f() { }

template<typename T, T N>
  requires N.x // { dg-error "enclose" }
void f() { }

template<typename T, T N>
  requires N->x && true // { dg-error "enclose" }
void f() { }

template<typename T, T N>
  requires N && N
void f() { }

template<typename T, T N>
  requires N || N
void f() { }

template<typename T, T N>
  requires N || !N // { dg-error "enclose" }
void f() { }

template<typename T, T N>
  requires N[0] // { dg-error "enclose" }
void f() { }

template<typename T, T N>
  requires static_cast<bool>(N) // { dg-error "enclose" }
void f() { }

