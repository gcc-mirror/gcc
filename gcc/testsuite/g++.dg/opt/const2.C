// PR optimization/6631

// { dg-do run }
// { dg-options "-O" }

extern "C" void abort (void);

struct QSize
{
  QSize();
  QSize( int w, int h );
  int wd, ht;
  friend inline const QSize operator+( const QSize &, const QSize & );
};

inline QSize::QSize()
{ wd = ht = -1; }

inline QSize::QSize( int w, int h )
{ wd = w; ht = h; }

inline const QSize operator+( const QSize & s1, const QSize & s2 )
{ return QSize(s1.wd+s2.wd, s1.ht+s2.ht); }

QSize minimumSize()
{
  return QSize (100, 200);
}

QSize totalMinimumSize()
{
    QSize s = minimumSize();
    return s + QSize( 0, 0 );
}

int main()
{
  QSize s = totalMinimumSize();
  if (s.wd != 100 || s.ht != 200)
    abort ();
}

