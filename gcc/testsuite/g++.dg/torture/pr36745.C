/* PR target/36745 */
/* { dg-do compile } */
/* { dg-options "-O2 -fPIC -Wno-return-type" } */
/* { dg-require-effective-target fpic } */

typedef __SIZE_TYPE__ size_t;
class QBasicAtomicInt
{
public:
  int _q_value;
  inline bool operator== (int value) const
  {
  }
  bool ref ();
  bool deref ();
};
inline bool
QBasicAtomicInt::ref ()
{
  __asm__ ("": "=m" (_q_value): :);
  return true;
}

namespace std
{
  using::size_t;
}
extern "C++"
{
  inline void *operator new (std::size_t, void *__p)
  {
    return __p;
  }
}
struct QMapData
{
  QBasicAtomicInt ref;
  static QMapData shared_null;
};
template < class Key, class T > class QMap
{
  QMapData *d;
public: inline QMap ():d (&QMapData::shared_null)
  {
  }
  inline ~ QMap ()
  {
    if (!d->ref.deref ())
      freeData (d);
  }
  void freeData (QMapData * d);
};
struct QVectorData
{
  QBasicAtomicInt ref;
  static QVectorData shared_null;
};
template < typename T > struct QVectorTypedData
{
  QBasicAtomicInt ref;
};
template < typename T > class QVector
{
  union
  {
    QVectorData *p;
    QVectorTypedData < T > *d;
  };
public: inline QVector ():p (&QVectorData::shared_null)
  {
    d->ref.ref ();
  }
  inline void detach ()
  {
    if (d->ref == 1)
      detach_helper ();
  }
  inline T *data ()
  {
    detach ();
  }
  T & operator[](int i);
  void detach_helper ();
  void realloc ();
};
template < typename T > void QVector < T >::detach_helper ()
{
  realloc ();
}

template < typename T > inline T & QVector < T >::operator[](int i)
{
  return data ()[i];
}
template < typename T > void QVector < T >::realloc ()
{
  T *j, *i;
  i->~T ();
  while (j-- == i)
    new (j) T;
}

void
mergeInto (QVector < int >*a)
{
};
struct QRegExpAutomatonState
{
  QVector < int >outs;
  QMap < int, int >reenter;
  QMap < int, int >anchors;
};
class QRegExpEngine
{
  void addCatTransitions ();
  QVector < QRegExpAutomatonState > s;
};
void
QRegExpEngine::addCatTransitions ()
{
  mergeInto (&s[2].outs);
}
