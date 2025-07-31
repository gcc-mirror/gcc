// { dg-do compile }
// { dg-require-effective-target fpic }
// { dg-require-effective-target tls }
// { dg-options "-O2 -fno-pic -fdump-ipa-whole-program" }
// { dg-add-options tls }

template<class T>
struct S {
  static __thread int i;
};

template<class T>
__thread int S<T>::i;

extern template
__thread int S<void>::i;

int &vi()
{
  return S<void>::i;
}

int &ci()
{
  return S<char>::i;
}

// tls_model should be tls-initial-exec due to extern template.
// { dg-final { scan-ipa-dump "Varpool flags: tls-initial-exec" "whole-program" } }
