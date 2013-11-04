// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin: PR c++/41785
// { dg-options -std=c++11 }

struct a {};

template < typename T, typename ENCLOSING >
struct base;

template < typename... T >
struct derived
  : public base< T, derived< T... > >...
{};

template < typename... T>
struct base< a, derived< T... > >
{
  typedef derived< T... >
          Derived;
};

int main()
{
  derived< a > instance;
}

