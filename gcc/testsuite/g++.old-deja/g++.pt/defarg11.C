// Build don't link:
// Origin: Jason Merrill <jason@cygnus.com>

template <class T, class U = int> struct A;
template <class T = int, class U> struct A;
