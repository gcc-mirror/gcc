// Build don't link:
#include <iostream.h>

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

    void        f () { cout << x << endl; }
};
