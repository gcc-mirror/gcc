// PR c++/29518

template< bool C > int assertion_failed( int);
template< class > 
struct N
{
  static bool const okay = true;
  enum {
    t = sizeof( assertion_failed<okay>( 0))
  };
};
int main()
{
  N<int> n;
}
