// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin PR c++/40007
// { dg-do compile }

class x
{
  typedef int privtype; // { dg-error "is private" "" { xfail *-*-* } }

protected:
  typedef int type;
};

template<typename T>
struct y : public x
{
  typename x::type z;
};

template<typename T>
struct y<T*> : public x
{ // { dg-error "within this context" "" { xfail *-*-* } }
  typedef x::type good;
  typedef x::privtype bad;
};

template class y<int>;
template class y<int*>;
