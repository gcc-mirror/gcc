template< int i > struct T :
public T< i-1 >
{
};

template<> struct T< 0 >
{
};

template< class F > struct T1 :
public T< F::dim >
{
};

template< int i > struct S
{
  enum { dim = i } ;
};

int main()
{
  T1< S< 4 > > t ;
  return( 0 ) ;
}

