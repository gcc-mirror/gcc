// { dg-do compile }
// { dg-require-effective-target fpic }
// { dg-require-visibility "" }
// { dg-options "-fPIC" }
/* { dg-additional-options "-Wno-return-type" } */


typedef __SIZE_TYPE__ size_t;
extern "C" void *
malloc (size_t __size)
throw () __attribute__ ((__malloc__));
     namespace std __attribute__ ((__visibility__ ("default")))
{
  using::size_t;
}
inline void *operator
new (std::size_t, void *__p)
throw ()
{
  return __p;
}
template < class _T1, class _T2 > struct pair
{
  _T1 first;
  _T2 second;
    pair (const _T1 & __a, const _T2 & __b):first (__a), second (__b)
  {
  }
  template < class _U1, class _U2 >
    pair (const pair < _U1, _U2 > &__p):first (__p.first), second (__p.second)
  {
  }
};

template < class _T1, class _T2 >
  inline pair < _T1, _T2 > make_pair (_T1 __x, _T2 __y)
{
  return pair < _T1, _T2 > (__x, __y);
}
template < typename _Tp > inline const _Tp &
max (const _Tp & __a, const _Tp & __b)
{
}
typedef unsigned short int uint16_t;
typedef unsigned long int uintptr_t;
typedef uint16_t UChar;
namespace std __attribute__ ((__visibility__ ("default")))
{
  struct __numeric_limits_base
  {
  };
  template < typename _Tp > struct numeric_limits:public __numeric_limits_base
  {
    static _Tp max () throw ()
    {
    }
  };
}

template < typename T > class VectorBufferBase
{
public:
  void allocateBuffer (size_t newCapacity)
  {
    if (newCapacity > std::numeric_limits < size_t >::max () / sizeof (T))
      *(int *) (uintptr_t) 0xbbadbeef = 0;
  }
};

template < typename T, size_t inlineCapacity > class VectorBuffer;
template < typename T > class VectorBuffer < T, 0 >:private VectorBufferBase <
  T >
{
public:
  typedef VectorBufferBase < T > Base;
  using Base::allocateBuffer;
};

template < typename T, size_t inlineCapacity = 0 > class Vector
{
  typedef VectorBuffer < T, inlineCapacity > Impl;
public:
  typedef T *iterator;
  size_t size () const
  {
    return m_size;
  }
  size_t capacity () const
  {
  }
  iterator begin ()
  {
  }
  iterator end ()
  {
    return begin () + m_size;
  }
  void shrink (size_t size);
  void reserveCapacity (size_t newCapacity);
  void clear ()
  {
    shrink (0);
  }
  template < typename U > void append (const U &);
  void expandCapacity (size_t newMinCapacity);
  template < typename U > U * expandCapacity (size_t newMinCapacity, U *);
  size_t m_size;
  Impl m_impl;
};
template < typename T, size_t inlineCapacity >
  void Vector < T, inlineCapacity >::expandCapacity (size_t newMinCapacity)
{
  reserveCapacity (max
		   (newMinCapacity,
		    max (static_cast < size_t > (16),
			 capacity () + capacity () / 4 + 1)));
}

template < typename T, size_t inlineCapacity >
  template < typename U >
  inline U * Vector < T,
  inlineCapacity >::expandCapacity (size_t newMinCapacity, U * ptr)
{
  expandCapacity (newMinCapacity);
}
template < typename T, size_t inlineCapacity >
  void Vector < T, inlineCapacity >::reserveCapacity (size_t newCapacity)
{
  m_impl.allocateBuffer (newCapacity);
}
template < typename T, size_t inlineCapacity >
  template < typename U >
  inline void Vector < T, inlineCapacity >::append (const U & val)
{
  const U *ptr = &val;
  if (size () == capacity ())
    ptr = expandCapacity (size () + 1, ptr);
  new (end ())T (*ptr);
}

class Range;
class TextIterator
{
public:
  explicit TextIterator (const Range *,
			 bool emitCharactersBetweenAllVisiblePositions =
			 false);
  bool atEnd () const
  {
  }
  void advance ();
  int length () const
  {
  }
};
UChar *
plainTextToMallocAllocatedBuffer (const Range * r, unsigned &bufferLength)
{
  static const unsigned cMaxSegmentSize = 1 << 16;
  typedef pair < UChar *, unsigned >TextSegment;
  Vector < TextSegment > *textSegments = 0;
  Vector < UChar > textBuffer;
  for (TextIterator it (r); !it.atEnd (); it.advance ())
    {
      if (textBuffer.size ()
	  && textBuffer.size () + it.length () > cMaxSegmentSize)
	{
	  UChar *newSegmentBuffer =
	    static_cast <
	    UChar * >(malloc (textBuffer.size () * sizeof (UChar)));
	  if (!textSegments)
	    textSegments = new Vector < TextSegment >;
	  textSegments->
	    append (make_pair (newSegmentBuffer, textBuffer.size ()));
	  textBuffer.clear ();
	}
    }
}
