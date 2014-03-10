// PR c++/43382
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
{ return deref(u.f(), args...); }

int main(void)
{
    Container<Container<int>> v;
    deref(v,1,2);
}
