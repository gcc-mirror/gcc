// Build don't link:
// Special g++ Options: -pedantic-errors

void cheat( int* i ) { ++(*i); }
 
struct t {
        void cheat( int& i ) { ++i; }
};

int main()
{
  void (t::*member)( const int& ) = &t::cheat; // ERROR - conversion
  void (*cheater)( const int* ) = &cheat; // ERROR - converting
  t t2;
  const int i=1;
  int j=1;
  (t2.*member)( i );
  (t2.*member)( j );
}
