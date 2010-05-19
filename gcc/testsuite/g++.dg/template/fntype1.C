bool f(int i) { return i != 5; }

template <class X, class P = bool(X)>
struct Traits
{
 typedef P type;
};

template <class X, class P = typename Traits<X>::type>
struct S
{
 const P& p_;
 S( const P& p ) : p_(p) {} // const reference
};

template <class X>
S<X> make_s(const typename Traits<X>::type & p) // const reference
{
 return S<X>(p); // << HERE
}


int main()
{
 make_s<int>(f);
}
