// PR c++/32756
// { dg-do compile }

// bogus overload warning

class QString;

struct QByteArray
{
  QByteArray ();
  bool operator!= (const QString & s2) const;
};

bool operator!= (const QByteArray & a1, const QByteArray & a2);

struct QString
{
  QString ();
  QString (const QByteArray & a);
};

QByteArray abbreviation ();

void
fromString ()
{
  QByteArray zoneAbbrev;
  if (abbreviation () != zoneAbbrev)
    {
    }
}
