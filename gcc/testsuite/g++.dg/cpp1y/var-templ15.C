// PR c++/63588
// { dg-do compile { target c++14 } }

template <class T> T elements;
int i = elements <>;		// { dg-error "arguments" }
