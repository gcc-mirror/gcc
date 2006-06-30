// PR c++/17470
// Test that we can give visibility to explicit template instantiations

// { dg-require-visibility "" }
// { dg-final { scan-hidden "_ZN1AIlE1fEl" } }
// { dg-final { scan-hidden "_ZN1AIiE1fEi" } }
// { dg-final { scan-not-hidden "_ZN1AIcE1fEc" } }
// { dg-final { scan-hidden "_Z8identityIdET_S0_" } }
// { dg-final { scan-not-hidden "_Z8identityIiET_S0_" } }

template <class T> T identity(T t) { return t; }
template  __attribute__((visibility("hidden"))) double identity(double);
template int identity(int);


template <class T> struct A { void f (T); };
template <class T> void A<T>::f (T) { }
template struct __attribute ((visibility ("hidden"))) A<int>;
template<> struct  __attribute ((visibility ("hidden"))) A<long> { void f(long); };
// inherits hidden visibility from its class
void A<long>::f (long) { }
template struct A<char>;
