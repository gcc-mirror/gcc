// Build don't link:
// Origin: Alex Samuel <samuel@codesourcery.com>

namespace NS 
{ 

template <class T, int V>
struct Base
{
};

template <class T>
struct Z
{
  const static int value_ = false;
};

template <class T>
struct A : 
  public Base <T, Z<T>::value_>
{
}; 

template <class T> 
void f(T)
{
}

}


template <template <class T> class U> 
struct B 
{
};


int 
main ()
{
  B<NS::A> ba; 
  f (ba);  // Koenig lookup
  return 0;
}

