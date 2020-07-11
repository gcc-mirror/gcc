// PR c++/94628
// A variant of variadic101.C where the recursive call to deref
// has its first template argument explicitly provided.
// { dg-do compile { target c++11 } }

template<class T>
struct Container
{ T f() const; };

template<class T>
T deref(const T& t)
{ return t; }


template <class T, class... Args>
auto
deref(const T& u, int r, Args... args)
-> decltype(deref(u.f(), args...))
{ return deref<decltype(u.f())>(u.f(), args...); }

int main(void)
{
    Container<Container<int>> v;
    deref(v,1,2);
}
