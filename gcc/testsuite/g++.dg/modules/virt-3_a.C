// PR c++/114229
// { dg-additional-options "-fmodules-ts -Wno-global-module" }
// { dg-module-cmi modA }

module;
template<class> struct basic_streambuf { virtual void overflow() { } };
extern template struct basic_streambuf<long>;
export module modA;
export basic_streambuf<long> *p;
