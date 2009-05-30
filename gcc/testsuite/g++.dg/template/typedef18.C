// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin PR c++/40007
// { dg-do compile }

template<typename T>
struct x
{
  protected:
  typedef int type;
};

template<typename T>
struct y : public x<T>
{
  typename x<T>::type z;
};

template<>
struct y<void> : public x<void>
{
  typedef x<void>::type z;
};

template class y<int>;
