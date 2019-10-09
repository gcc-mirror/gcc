// { dg-do compile { target c++2a } }

template <class T> concept has_mem_type = requires { typename T::type; };

template <has_mem_type T> int f () { return 0; }
template <class T> char f() { return 0; }

struct A;
static_assert (sizeof (f<A>()) == 1);
struct A { typedef int type; };
static_assert (sizeof (f<A>()) > 1);
