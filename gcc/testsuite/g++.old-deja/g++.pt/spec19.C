// { dg-do assemble  }

template<class T> T f(T o) { return o; }
template<> int f(int o)    { return o; }
template int f(int);
