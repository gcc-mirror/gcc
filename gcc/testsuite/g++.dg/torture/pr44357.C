/* { dg-do compile } */
/* { dg-additional-options "-Wno-return-type" } */

extern "C"
{
  typedef long unsigned int size_t;
}
namespace llvm
{
  namespace dont_use
  {
    template < typename T > double is_class_helper (...);
  }
  template < typename T > struct is_class
  {
  public:enum
    { value = sizeof (char) == sizeof (dont_use::is_class_helper < T > (0)) };
  };
    template < typename T > struct isPodLike
  {
    static const bool value = !is_class < T >::value;
  };
}
namespace std __attribute__ ((__visibility__ ("default")))
{
  template < typename _Iterator > struct iterator_traits
  {
  };
  template < typename _Tp > struct iterator_traits <_Tp * >
  {
    typedef _Tp value_type;
  };
}

namespace __gnu_cxx __attribute__ ((__visibility__ ("default")))
{
  template < typename _Tp > class new_allocator
  {
  public:typedef size_t size_type;
    typedef const _Tp & const_reference;
  };
}

namespace std __attribute__ ((__visibility__ ("default")))
{
template < typename _Tp > class allocator:public __gnu_cxx::new_allocator <
    _Tp >
  {
  public:typedef size_t size_type;
    template < typename _Tp1 > struct rebind
    {
      typedef allocator < _Tp1 > other;
    };
  };
  template < typename _Tp, typename _Alloc > struct _Vector_base
  {
    typedef typename _Alloc::template rebind < _Tp >::other _Tp_alloc_type;
  };
template < typename _Tp, typename _Alloc = std::allocator < _Tp > >class vector:protected _Vector_base < _Tp,
    _Alloc
    >
  {
    typedef _Vector_base < _Tp, _Alloc > _Base;
    typedef typename _Base::_Tp_alloc_type _Tp_alloc_type;
  public:typedef _Tp value_type;
    typedef typename _Tp_alloc_type::const_reference const_reference;
    typedef size_t size_type;
    size_type size () const
    {
    } const_reference operator[] (size_type __n) const
    {
  }};
}

namespace llvm
{
  struct LandingPadInfo;
  class DwarfException
  {
    static bool PadLT (const LandingPadInfo * L, const LandingPadInfo * R);
    struct CallSiteEntry
    {
    };
    void EmitExceptionTable ();
  };
}
namespace std __attribute__ ((__visibility__ ("default")))
{
  template < typename _RandomAccessIterator,
    typename _Compare >
    void __unguarded_linear_insert (_RandomAccessIterator __last,
				    _Compare __comp)
  {
    typename iterator_traits < _RandomAccessIterator >::value_type __val =
      (*__last);
    _RandomAccessIterator __next = __last;
    while (__comp (__val, *__next))
      {
      }
  }
  template < typename _RandomAccessIterator,
    typename _Compare > void __insertion_sort (_RandomAccessIterator __first,
					       _RandomAccessIterator __last,
					       _Compare __comp)
  {
    for (_RandomAccessIterator __i = __first + 1; __i != __last; ++__i)
      {
	if (__comp (*__i, *__first))
	  {
	  }
	else
	  std::__unguarded_linear_insert (__i, __comp);
      }
  }
  enum
  { _S_threshold = 16 };
  template < typename _RandomAccessIterator,
    typename _Compare >
    void __final_insertion_sort (_RandomAccessIterator __first,
				 _RandomAccessIterator __last,
				 _Compare __comp)
  {
    if (__last - __first > int (_S_threshold))
      {
	std::__insertion_sort (__first, __first + int (_S_threshold), __comp);
      }
  }
  template < typename _RandomAccessIterator,
    typename _Compare > inline void sort (_RandomAccessIterator __first,
					  _RandomAccessIterator __last,
					  _Compare __comp)
  {
    if (__first != __last)
      {
	std::__final_insertion_sort (__first, __last, __comp);
      }
  }
}

namespace llvm
{
  class SmallVectorBase
  {
  protected:void *BeginX, *EndX, *CapacityX;
    struct U
    {
    } FirstEl;
  protected:  SmallVectorBase (size_t Size):BeginX (&FirstEl), EndX (&FirstEl),
      CapacityX ((char *) &FirstEl + Size)
    {
  }};
template < typename T > class SmallVectorTemplateCommon:public
    SmallVectorBase
  {
  public: SmallVectorTemplateCommon (size_t Size):SmallVectorBase (Size)
    {
    } typedef size_t size_type;
    typedef T *iterator;
    iterator begin ()
    {
    } iterator end ()
    {
    } size_type size () const
    {
  }};
template < typename T, bool isPodLike > class SmallVectorTemplateBase:public SmallVectorTemplateCommon <
    T >
  {
  public: SmallVectorTemplateBase (size_t Size):SmallVectorTemplateCommon < T >
      (Size)
    {
  }};
template < typename T > class SmallVectorImpl:public SmallVectorTemplateBase < T,
    isPodLike < T >::value >
  {
    typedef SmallVectorTemplateBase < T, isPodLike < T >::value > SuperClass;
  public:typedef typename SuperClass::iterator iterator;
    explicit SmallVectorImpl (unsigned N):SmallVectorTemplateBase < T,
      isPodLike < T >::value > (N * sizeof (T))
    {
    }
  };
  template < typename T,
    unsigned N > class SmallVector:public SmallVectorImpl < T >
  {
    typedef typename SmallVectorImpl < T >::U U;
    enum
    { MinUs =
	(static_cast < unsigned int >(sizeof (T)) * N + static_cast <
	 unsigned int >(sizeof (U)) - 1) /static_cast <
	unsigned int >(sizeof (U)), NumInlineEltsElts =
	MinUs > 1 ? (MinUs - 1) : 1, NumTsAvailable =
	(NumInlineEltsElts + 1) * static_cast <
	unsigned int >(sizeof (U)) / static_cast <
	unsigned int >(sizeof (T)) };
  public:  SmallVector ():SmallVectorImpl < T > (NumTsAvailable)
    {
    }
  };
  struct LandingPadInfo
  {
    std::vector < int >TypeIds;
    union
    {
    } Contents;
  };
}

using namespace llvm;
bool
DwarfException::PadLT (const LandingPadInfo * L, const LandingPadInfo * R)
{
  const std::vector < int >&LIds = L->TypeIds, &RIds = R->TypeIds;
  unsigned LSize = LIds.size (), RSize = RIds.size ();
  unsigned MinSize = LSize < RSize ? LSize : RSize;
  for (unsigned i = 0; i != MinSize; ++i)
    if (LIds[i] != RIds[i])
      return LIds[i] < RIds[i];
}

void
DwarfException::EmitExceptionTable ()
{
  SmallVector < const LandingPadInfo *, 64 > LandingPads;
  std::sort (LandingPads.begin (), LandingPads.end (), PadLT);
  SmallVector < CallSiteEntry, 64 > CallSites;
  for (unsigned i = 0, e = CallSites.size (); i < e; ++i)
    {
    }
}
