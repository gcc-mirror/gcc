// { dg-do assemble  }
#include <iostream>

template < class T >
class X
{
  protected:

    union {
        int     x;
        double  y;
    };
};

template < class T >
class Y : public X<T>
{
  public:

    using X<T>::x;

    void        f () { std::cout << this->x << std::endl; }
};
