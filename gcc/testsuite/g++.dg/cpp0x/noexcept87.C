// PR c++/115223
// { dg-do compile { target c++11 } }
// { dg-additional-options -flto }

template<class T>
void f() noexcept(bool(T() || true));

void g() { f<int>(); }

using type = void;
type callDestructorIfNecessary() noexcept {}
