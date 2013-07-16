// PR c++/55232
// { dg-do compile { target c++11 } }

struct vector
{
    typedef int value_type;
};

template< class U, class... T >
struct X
{
    void push_back( typename T::value_type ... vals )
    {
      U::asoeuth;		// { dg-error "" }
    }
};

int main()
{
  X< int, vector > x;
  x.push_back( 0 );
}
