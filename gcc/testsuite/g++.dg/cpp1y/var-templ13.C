// { dg-do compile { target c++14 } }

template <class T> T x;
template <> int x<int> = 0;
template <> int x<int> = 0;	// { dg-error "x<int>" }
