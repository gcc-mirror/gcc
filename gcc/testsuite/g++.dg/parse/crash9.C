// { dg-do compile }

// Origin: Volker Reichelt <reichelt@gcc.gnu.org>

// PR c++/10793: ICE in handling base class when the current class
// contains error.

template <typename> struct A {};
template <typename> struct A<INVALID> : A<int> { }; // { dg-error "not declared|invalid|token|extra" }
