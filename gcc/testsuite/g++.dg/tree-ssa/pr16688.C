/* PR 16688.  Test provided by Wolfgang Bangerth.  The alias analyzer
   was aborting when trying to group aliases.  */

/* { dg-do compile } */
/* { dg-options "-O2" } */

template<typename _Tp>
struct iterator_traits;

template<typename _Tp>
struct iterator_traits<_Tp*> {
    typedef _Tp& reference;
};

template<typename _Iterator> struct NI {
    _Iterator current;

    typedef typename iterator_traits<_Iterator>::reference reference;

    NI() { }

    NI(const _Iterator& __i) : current(__i) { }

    reference operator*() const { return *current; }

    NI& operator++() { return *this; }

    const _Iterator& base() const { return current; }
};

template<typename _IteratorL, typename _IteratorR>
inline int
operator-(const NI<_IteratorL>& __lhs,
          const NI<_IteratorR>& __rhs)
{ return __lhs.base() - __rhs.base(); }


template<typename _II, typename _OI>
inline _OI
__copy_aux(_II __first, _II __last, _OI __result)
{
  struct __copy {
      static _OI
      copy(_II __first, _II __last, _OI __result)
        {
          for (; __first != __last; ++__result, ++__first)
            *__result = *__first;
          return __result;
        }
  };

  return __copy::copy(__first, __last, __result);
}

struct __copy_normal
{
    template<typename _II, typename _OI>
    static _OI
    copy_n(_II __first, _II __last, _OI __result)
      {
        return __copy_aux(__first, __last, __result);
      }
};
template<typename _InputIterator, typename _OutputIterator>
inline _OutputIterator
copy(_InputIterator __first, _InputIterator __last,
     _OutputIterator __result)
{
  return __copy_normal::copy_n(__first, __last, __result);
}

template <typename T, typename U, typename V>
void uninitialized_fill_n(T,U,V);


template<typename _Tp>
struct _Vector_base {
    struct _Vector_impl {
        _Tp* start;
        _Tp* finish;
        _Tp* end_of_storage;
        _Vector_impl() : start(0), finish(0), end_of_storage(0)
          { }
    } impl;

    _Vector_base(unsigned __n) {
      impl.start = allocate(__n);
      impl.finish = impl.start;
      impl.end_of_storage = impl.start + __n;
    }

    ~_Vector_base() {
      deallocate(impl.start,
                    impl.end_of_storage - impl.start);
    }

    _Tp* allocate(unsigned __n);

    void deallocate(_Tp* __p, unsigned __n);

    NI<_Tp*> begin() { return NI<_Tp*> (impl.start); }
};


template<typename _Tp>
struct vector : _Vector_base<_Tp>
{
    vector(int __n)
                    : _Vector_base<_Tp>(__n)
      {
        uninitialized_fill_n(this->impl.start, __n, _Tp());
      }
};



struct Tensor
{
    Tensor ();
    Tensor (const Tensor &);

    double values[2];
};


inline
Tensor::Tensor (const Tensor &p)
{
  for (unsigned int i=0; i<2; ++i)
    values[i] = p.values[i];
}


struct TriaAccessor
{
    typedef void * AccessorData;

    void copy_from (const TriaAccessor &);
    void operator = (const TriaAccessor *);

    TriaAccessor & operator = (const TriaAccessor &);

    bool operator == (const TriaAccessor &) const;

    bool operator != (const TriaAccessor &) const;
    void operator ++ ();

    int state () const;
    bool used () const;

    int present_level;
    int present_index;
    int** levels;
};

inline int TriaAccessor::state () const {
  if ((present_level>=0) && (present_index>=0))
    return 0;
  else
    if ((present_level==-1) && (present_index==-1))
      return 1;
    else
      return 2;
}


inline
void TriaAccessor::operator ++ () {
  ++this->present_index;

  while (this->present_index >=
         static_cast<int>(*this->levels[this->present_level]))
    {
      ++this->present_level;
      this->present_index = 0;

      if (this->present_level >= static_cast<int>(1))
        {

          this->present_level = this->present_index = -1;
          return;
        }
    }
}

struct MGDoFObjectAccessor : TriaAccessor {};



struct TriaRawIterator
{
    TriaRawIterator ();

    TriaRawIterator (const TriaRawIterator &);
    TriaRawIterator (const MGDoFObjectAccessor &a);
    const MGDoFObjectAccessor & operator * () const;
    
    MGDoFObjectAccessor & operator * ();
    const MGDoFObjectAccessor * operator -> () const;

    MGDoFObjectAccessor * operator -> ();

    TriaRawIterator & operator = (const TriaRawIterator &);

    bool operator == (const TriaRawIterator &) const;
    bool operator != (const TriaRawIterator &) const;
    bool operator < (const TriaRawIterator &) const;
    MGDoFObjectAccessor accessor;

    TriaRawIterator & operator ++ ();
};

struct TriaIterator : TriaRawIterator
{
    TriaIterator ();

    TriaIterator (const TriaIterator &i);

    TriaIterator &
    operator = (const TriaIterator &);

    TriaIterator &
    operator = (const TriaRawIterator &);
};


inline
TriaRawIterator::TriaRawIterator (const TriaRawIterator &i) :
                accessor (i.accessor) {}

inline
TriaIterator::TriaIterator (const TriaIterator &i) :
                TriaRawIterator (static_cast<TriaRawIterator >(i)) {}

inline
TriaRawIterator & TriaRawIterator::operator ++ () {
  while (++accessor, (this->accessor.state() == 0))
    if (this->accessor.used() == true)
      return *this;
  return *this;
}

struct Comp {
    Comp (const Tensor &dir) : dir(dir) {}

    bool operator () (const TriaIterator &c1, const TriaIterator &c2) const;
    const Tensor dir;
};


template<typename Iter>
void x1(Iter first, Iter last, int i, Comp comp)
{
  x1(Iter(), last, i, comp);
}

template<typename Iter>
inline void x2(Iter first, Iter last, Comp comp)
{
  if (first.base() != last.base())
    x1(first, last, (last - first), comp);
}

void downstream_dg (const Tensor& direction)
{
  vector<TriaIterator> ordered_cells(13);
  const Comp comparator(direction);

  TriaIterator begin, end;

  copy (begin, end, ordered_cells.begin());
  x2 (ordered_cells.begin(), ordered_cells.begin(), comparator);
}
