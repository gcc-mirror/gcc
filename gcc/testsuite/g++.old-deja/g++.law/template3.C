// Build don't link: 
// GROUPS passed templates
template< class R, class T1 = R, class T2 = T1 >
struct plus
  {
  R operator()( const T1& x, const T2& y ) const
    {
    return x + y;
    }
  };

int
main()
  {
  plus< int > p;
  return 0;
  }
