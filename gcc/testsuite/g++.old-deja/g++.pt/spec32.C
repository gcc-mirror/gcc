// Build don't link:
// Origin: Raja R Harinath <harinath@cs.umn.edu>

template<class T1, class T2> class foo;
template<class T> struct foo<T,typename T::bar>;
