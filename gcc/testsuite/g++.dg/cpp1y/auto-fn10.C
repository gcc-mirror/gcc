// A template declared with auto should be declared with auto in an
// explicit instantiation or explicit specialization, too.
// { dg-do compile { target c++14 } }

template <class T>
auto f(T t) { return t; }

template<> auto f<int>(int);
template auto f<float>(float);
template<> auto f(int*);
template auto f(float*);

template<> short f<short>(short); // { dg-error "does not match" }
template char f<char>(char);	  // { dg-error "does not match" }
template<> short f(short*);	  // { dg-error "does not match" }
template char f(char*);		  // { dg-error "does not match" }
