/* { dg-do compile } */

void *fastMalloc (int n);
void fastFree (void *p);
template <class T> struct C
{
  void deref () { delete static_cast <T *>(this); }
};
template <typename T>
struct D
{
  D (T *ptr) : m_ptr (ptr) { }
  ~D () { if (T * ptr = m_ptr) ptr->deref (); }
  T *operator-> () const;
  T *m_ptr;
  typedef T *UnspecifiedBoolType;
  operator UnspecifiedBoolType () const;
};
template <typename T> struct E
{
  static void destruct (T * begin, T * end)
    {
      for (T * cur = begin; cur != end; ++cur)
	cur->~T ();
    }
};
template <typename T> class F;
template <typename T> struct G
{
  static void destruct (T * begin, T * end)
    {
      E <T>::destruct (begin, end);
    }
  static void uninitializedFill (T * dst, T * dstEnd, const T & val)
    {
      F<T>::uninitializedFill (dst, dstEnd, val);
    }
};
template <typename T> struct H
{
  void allocateBuffer (int newCapacity)
    {
      m_buffer = static_cast <T *>(fastMalloc (newCapacity * sizeof (T)));
    }
  void deallocateBuffer (T * bufferToDeallocate)
    {
      if (m_buffer == bufferToDeallocate)
	fastFree (bufferToDeallocate);
    }
  T *buffer () { }
  int capacity () const { }
  T *m_buffer;
};
template <typename T, int cap> class I;
template <typename T> struct I <T, 0> : H <T>
{
  I (int capacity) { allocateBuffer (capacity); }
  ~I () { this->deallocateBuffer (buffer ()); }
  using H <T>::allocateBuffer;
  H <T>::buffer;
};
template <typename T, int cap = 0> struct J
{
  typedef T *iterator;
  ~J () { if (m_size) shrink (0); }
  J (const J &);
  int capacity () const { m_buffer.capacity (); }
  T & operator[](int i) { }
  iterator begin () { }
  iterator end () { return begin () + m_size; }
  void shrink (int size);
  template <typename U> void append (const U &);
  int m_size;
  I <T, cap> m_buffer;
};
template <typename T, int cap>
J <T, cap>::J (const J & other) : m_buffer (other.capacity ())
{
}
template <typename T, int cap>
void J <T, cap>::shrink (int size)
{
  G <T>::destruct (begin () + size, end ());
  m_size = size;
}
struct A : public C <A>
{
  virtual ~A ();
  typedef J <D <A> > B;
  virtual A *firstChild () const;
  virtual A *nextSibling () const;
  virtual const B & children (int length);
  B m_children;
};
const A::B &
A::children (int length)
{
  for (D <A> obj = firstChild (); obj; obj = obj->nextSibling ())
    {
      B children = obj->children (2);
      for (unsigned i = 0; i <length; ++i)
	m_children.append (children[i]);
    }
}

