// Build don't link: 
// GROUPS passed scope pt
class Link {
public:
  Link();
  Link(Link *);
private:
  Link *next_;

friend class IListBase;
friend class IListIterBase;
};

inline
Link::Link() : next_(0)
{
}

inline
Link::Link(Link *next) : next_(next)
{
}

class IListBase {
public:
  IListBase();
  IListBase(Link *);
  void  append(Link *);
  void insert(Link *);
  Link *head();
  int empty();
  Link *get();
  void remove(Link *);
private:
  Link *head_;
friend class IListIterBase;
};

inline
IListBase::IListBase() : head_(0)
{
}

inline
IListBase::IListBase(Link *head) : head_(head)
{
}

inline
void IListBase::insert(Link *p)
{
  p->next_ = head_;
  head_ = p;
}

inline
Link *IListBase::head()
{
  return head_;
}

inline
int IListBase::empty()
{
  return head_ == 0;
}

inline
Link *IListBase::get()
{
  Link *tem = head_;
  head_ = head_->next_;
  return tem;
}
  
template<class T> class IListIter;

template<class T>
class IList : private IListBase {
public:
  IList() { }
  IList(T *p) : IListBase(p) { }
  ~IList();
  void append(T *p) { IListBase::append(p); }
  void insert(T *p) { IListBase::insert(p); }
  void remove(T *p) { IListBase::remove(p); }
  T *head() { return (T *)IListBase::head(); }
  T *get() { return (T *)IListBase::get(); }
  IListBase::empty;
friend class IListIter<T>;
};

template<class T>
IList<T>::~IList()
{
  while (!empty())
    delete get();
}

class IListIterBase {
public:
  IListIterBase(const IListBase &);
  int done();
  Link *cur();
  void next();
private:
  Link *p_;
};

inline
IListIterBase::IListIterBase(const IListBase &list) : p_(list.head_)
{
}

inline
int IListIterBase::done()
{
  return p_ == 0;
}

inline
Link *IListIterBase::cur()
{
  return p_;
}

inline
void IListIterBase::next()
{
  p_ = p_->next_;
}


template<class T>
class IListIter : private IListIterBase {
public:
  IListIter(const IList<T> &list) : IListIterBase(list) { }
  T *cur() { return (T *)IListIterBase::cur(); }
  IListIterBase::next;
  IListIterBase::done;
};


struct A {
  IList<Link> list;
  int x;
  void foo();
};


void A::foo()
{
  for (IListIter<Link> iter(list); !iter.done(); iter.next())
    ;
  x = 0;
}
