// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin PR c++/40007
// { dg-do compile }

class x
{
  typedef int privtype; // { dg-message "private" }

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
{
  typedef x::type good;
  typedef x::privtype bad; // { dg-error "within this context" }
};

template class y<int>;
template class y<int*>;
