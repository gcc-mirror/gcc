/* This used to ICE due to a backend problem on s390.  */

/* { dg-do compile } */
/* { dg-options "-O1" } */

class ReferenceCounted
{
public:

  virtual ~ ReferenceCounted ()
  {
  }
  void decrementRefCount () const
  {
    if (--const_cast < unsigned int &>(_ref_count) == 0)
      {
	delete this;
      }
  }
  unsigned int _ref_count;
};

template < class T > class RefCountPointer
{
public:

RefCountPointer (T * p = 0):_p (p)
  {
  }
  RefCountPointer & operator= (const RefCountPointer < T > &o)
  {
    if (_p != o._p)
      {
	if (_p != 0)
	  _p->decrementRefCount ();
      }
  }
  ~RefCountPointer ()
  {
  }
  T *_p;
};
class Item:public ReferenceCounted
{
public:

  typedef RefCountPointer < const Item > Ptr;
};
class AnyAtomicType:public Item
{
};
class StaticContext
{
};
class DynamicContext:public StaticContext
{
};
class SortableItem
{
  SortableItem ();
  int m_bAscending:1;
  DynamicContext *m_context;
    AnyAtomicType::Ptr m_item;
};
SortableItem::SortableItem ()
{
  m_context = __null;
  m_item = __null;
}
