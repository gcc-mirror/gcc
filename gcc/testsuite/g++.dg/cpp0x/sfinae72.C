// PR c++/113789
// { dg-do compile { target c++11 } }
// Like sfinae70.C but ().

struct AutoPtr {
    AutoPtr() = default;
    AutoPtr(AutoPtr&) {}
};

template<class T> auto f(T p, int) -> decltype(throw (p), 1) = delete;
template<class T> void f(T p, long);

void
g ()
{
  f (AutoPtr (), 42); // { dg-error "use of deleted function" "" { target c++20_down } }
}
