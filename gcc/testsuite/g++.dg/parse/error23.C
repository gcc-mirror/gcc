// PR c++/19149

struct QChar {
  QChar( char c );
  QChar( const QChar& c );
  static const ; // { dg-error "" }
};
