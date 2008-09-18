// { dg-options "-fshow-column -ansi -pedantic-errors -Wno-long-long" }
// PR c++/19149

struct QChar {
  QChar( char c );
  QChar( const QChar& c );
  //following column number is not accurate enough but will make it for now
  static const ; // { dg-error "10:declaration does not declare anything" }
};
