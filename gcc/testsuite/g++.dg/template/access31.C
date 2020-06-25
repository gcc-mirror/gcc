// PR c++/47346
// { dg-do compile }

class C
{
  struct Private { };
};

template<typename T>
struct exploit1
{
    typedef C::Private type; // { dg-error "private" }
};
exploit1<int>::type x1;

template<typename T>
struct exploit2 : C::Private // { dg-error "private" }
{
};
exploit2<int> x2;

template<typename T>
struct exploit3
{
    template<class U = C::Private> // { dg-error "private" }
    struct E {};
};

exploit3<int>::E<> e;
