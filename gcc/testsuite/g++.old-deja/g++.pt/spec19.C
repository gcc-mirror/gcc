// Build don't link:

template<class T> T f(T o) { return o; }
template<> int f(int o)    { return o; } // ERROR - after specialization
template int f(int);  // ERROR - explicit instantiation
