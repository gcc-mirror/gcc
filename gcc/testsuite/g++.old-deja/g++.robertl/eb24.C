// Build don't link:
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

    void        f () { std::cout << x << std::endl; }
};
