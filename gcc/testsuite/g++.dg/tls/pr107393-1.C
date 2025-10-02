// { dg-do compile { target c++11 } }
// { dg-require-effective-target fpic }
// { dg-require-effective-target tls }
// { dg-options "-O2 -fno-pic -fdump-ipa-whole-program" }
// { dg-add-options tls }

struct Dtor;
template <typename> struct X { static thread_local Dtor m; };
template <typename T> thread_local Dtor X<T>::m;
extern template Dtor X<char>::m;
void *e2 = &X<char>::m;

// tls_model should be tls-initial-exec due to extern template.
// { dg-final { scan-ipa-dump "Varpool flags: tls-initial-exec" "whole-program" } }
