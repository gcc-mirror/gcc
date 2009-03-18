/* { dg-options "-O2 -fgraphite-identity" } */

struct Point
{
  int line, col;

  Point( int l = -1, int c = 0 ) throw() : line( l ), col( c ) {}
  bool operator==( const Point & p ) const throw()
  { return ( line == p.line && col == p.col ); }
  bool operator<( const Point & p ) const throw()
  { return ( line < p.line || ( line == p.line && col < p.col ) ); }
};

class Buffer
{
public:
  int characters( const int line ) const throw();
  int pgetc( Point & p ) const throw();
  Point eof() const throw() { return Point( 0, 0 ); }
  bool pisvalid( const Point & p ) const throw()
  { return ( ( p.col >= 0 && p.col < characters( p.line ) ) || p == eof() );
  }
  bool save( Point p1 = Point(), Point p2 = Point() ) const;
};

bool Buffer::save( Point p1, Point p2 ) const
{
  if( !this->pisvalid( p1 ) ) p1 = eof();
  if( !this->pisvalid( p2 ) ) p2 = eof();
  for( Point p = p1; p < p2; ) { pgetc( p ); }
  return true;
}


