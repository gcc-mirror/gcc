/* { dg-do compile } */
/* { dg-options "-O3 -std=c++11" } */
/* { dg-final { scan-assembler "_ZN5QListI7QStringED1Ev" { target comdat_group } } } */

class A
{
public:
  bool deref ();
};
class QString;
struct B
{
  A ref;
};
template <typename> class QList
{
  B d;
public:
  ~QList ();
  class const_iterator
  {
  };
  const_iterator constBegin ();
  void clear ();
  void dealloc ();
};
template <typename T> QList<T>::~QList ()
{
  if (d.ref.deref ())
    dealloc ();
}
template <typename T>
void
QList<T>::clear ()
{
  QList ();
}
class A1 : public QList<QString>
{
};
class B1
{
public:
  B1 (A1);
};
struct F
{
  void addMatch (const QString &&);
  A1 m_matchingMimeTypes;
};
class G
{
  A1 matchingGlobs (const QString &) const;
};
void
F::addMatch (const QString &&)
{
  m_matchingMimeTypes.clear ();
}
A1
G::matchingGlobs (const QString &) const
{
  A1 a;
  for (B1 b (a);;)
    ;
}
