template<typename T> class Base;

template<typename U> class Derived : Base<U>
{
  using Base_ = Base<U>;
  using typename Base_::base_member;

public:
  base_member Func ();
};
