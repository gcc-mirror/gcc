// N3323

#define assert(E) if(!(E))__builtin_abort();

template<class T>
class zero_init
{
public:
  zero_init( )
    : val( static_cast<T>(0) ) { }
  zero_init( T val ) : val( val )
  { }
  operator T & ( ) { return val; }
  operator T ( ) const { return val; }
private:
  T val;
};

void f()
{
  zero_init<int*> p; assert( p == 0 );
  p = new int(7);
  assert( *p == 7 );
  delete p; // error!

  zero_init<int> i; assert( i == 0 );
  i = 7;
  assert( i == 7 );
  switch( i ) {  } // error!

  int *vp = new int[i];
}
