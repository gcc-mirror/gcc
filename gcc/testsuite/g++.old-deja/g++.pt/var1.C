// { dg-do assemble  }
// Origin: Jason Merrill <jason@cygnus.com>

template <class T> T t; // { dg-error "" "" { target { ! c++14 } } } template declaration of t
