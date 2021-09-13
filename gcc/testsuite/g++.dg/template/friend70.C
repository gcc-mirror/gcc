// PR c++/52625

template<class>
class base {};

class derived : public base<derived>
{
  template<class> friend class base;
};
