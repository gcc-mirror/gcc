// checking ICE in canonical typing

class A;

template <typename> struct B
{
  typedef A type;
};

template <class T> class C
  : public B<T>::type
{
} __attribute__ ((__may_alias__));

class A
{
  operator const C<int> &()
  {
    return *static_cast<const C<int> *> (this);
  }
};
