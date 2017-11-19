// { dg-do compile }
// { dg-options "-O3 -floop-interchange -Wno-conversion-null -Wno-return-type" }

void __throw_bad_alloc ();

template <typename _Tp> void
swap (_Tp & __a, _Tp __b)
{
  __a = __b;
}

template <typename _Category> struct iterator
{
  typedef _Category iterator_category;
};

template <typename _Tp> struct allocator
{
  typedef __SIZE_TYPE__ size_type;
  typedef _Tp pointer;
  pointer allocate (size_type)
  {
    __throw_bad_alloc ();
    return __null;
  }
};

template <class T, class = allocator <T> >class unbounded_array;
template <class T, class = unbounded_array <T> >class vector;
template <class = int> class scalar_vector;
template <class IC> struct random_access_iterator_base : public iterator <IC>
{
};

template <class X, class> struct promote_traits
{
  typedef __typeof__ ((X ())) promote_type;
};

template <class T> struct scalar_traits
{
  typedef T const_reference;
  typedef T reference;
};

template <class T> struct type_traits : scalar_traits <T>
{
};

struct dense_proxy_tag
{
};

template <class> struct iterator_base_traits;

template <> struct iterator_base_traits <dense_proxy_tag>
{
  template <class, class> struct iterator_base
  {
    typedef random_access_iterator_base <dense_proxy_tag> type;
  };
};

template <class I1, class> struct iterator_restrict_traits
{
  typedef I1 iterator_category;
};

template <class> class storage_array
{
};

template <class T, class ALLOC> struct unbounded_array : public storage_array <unbounded_array <ALLOC> >
{
  typedef typename ALLOC::size_type size_type;
  typedef T & reference;
  typedef T *pointer;
  unbounded_array (size_type size, ALLOC = ALLOC ()) : alloc_ (), size_ (size)
  {
    alloc_.allocate (size_);
  }
  ~unbounded_array ()
  {
    if (size_)
      for (;;);
  }
  size_type
  size () const
  {
    return size_;
  }
  reference
  operator[] (size_type i)
  {
    return data_[i];
  }
  void
  swap (unbounded_array & a)
  {
    ::swap (size_, a.size_);
  }
  ALLOC alloc_;
  size_type size_;
  pointer data_;
};

template <class T1, class T2> struct scalar_binary_functor
{
  typedef typename promote_traits <T1, T2>::promote_type result_type;
};

template <class T1, class T2> struct scalar_plus : public scalar_binary_functor <T1, T2>
{
};

template <class T1, class T2> struct scalar_multiplies : public scalar_binary_functor <T1, T2>
{
};

template <class T1, class T2> struct scalar_binary_assign_functor
{
  typedef typename type_traits <T1>::reference argument1_type;
  typedef typename type_traits <T2>::const_reference argument2_type;
};

template <class T1, class T2> struct scalar_assign : public scalar_binary_assign_functor <T1, T2>
{
  typedef typename scalar_binary_assign_functor <T1, T2>::argument1_type argument1_type;
  typedef typename scalar_binary_assign_functor <T1, T2>::argument2_type argument2_type;
  static const bool computed = false;
  static void
  apply (argument1_type t1, argument2_type t2)
  {
    t1 = t2;
  }
};

template <class E> struct vector_expression
{
  typedef E expression_type;
  const expression_type &
  operator () () const
  {
    return *static_cast <const expression_type *>(this);
  }
};

template <class C> class vector_container : public vector_expression <C>
{
};

template <class E> struct vector_reference : public vector_expression <vector_reference <E> >
{
  typedef typename E::size_type size_type;
  typename E::const_reference const_reference;
  typedef E referred_type;
  vector_reference (referred_type & e) : e_ (e)
  {
  }
  size_type
  size () const
  {
    return expression ().size ();
  }
  referred_type &
  expression () const
  {
    return e_;
  }
  referred_type &e_;
};

template <class E1, class E2, class F> struct vector_binary : public vector_expression <vector_binary <E1, E2, F> >
{
  typedef E1 expression1_type;
  typedef E2 expression2_type;
  typedef typename E1::const_closure_type expression1_closure_type;
  typedef typename E2::const_closure_type expression2_closure_type;
  typedef typename promote_traits <typename E1::size_type, typename E2::size_type>::promote_type size_type;
  typedef typename F::result_type value_type;

  vector_binary (const expression1_type & e1, expression2_type e2) : e1_ (e1), e2_ (e2)
  {
  }

  size_type
  size () const
  {
    return e1_.size ();
  }

  class const_iterator : public iterator_base_traits <typename iterator_restrict_traits <typename E1::const_iterator::iterator_category, const_iterator>::iterator_category>::template iterator_base <const_iterator, value_type>::type
  {
  };
  expression1_closure_type e1_;
  expression2_closure_type e2_;
};

template <class E1, class E2, class F> struct vector_binary_traits
{
  typedef vector_binary <E1, E2, F> expression_type;
  typedef expression_type result_type;
};

template <class E1, class E2> typename vector_binary_traits <E1, E2, scalar_plus <typename E1::value_type, typename E2::value_type> >::result_type
operator + (vector_expression <E1> &e1, const vector_expression <E2> &e2)
{
  typedef typename vector_binary_traits <E1, E2, scalar_plus <typename E1::value_type, typename E2::value_type> >::expression_type expression_type;
  return expression_type (e1 (), e2 ());
}

template <class E1, class E2, class F> struct vector_binary_scalar2 : public vector_expression <vector_binary_scalar2 <E1, E2, F> >
{
  typedef vector_binary_scalar2 <E1, E2, F> self_type;
  typedef typename E1::size_type size_type;
  typedef typename F::result_type value_type;
  typedef self_type const_closure_type;
};

template <class E1, class E2, class F> struct vector_binary_scalar2_traits
{
  typedef vector_binary_scalar2 <E1, E2, F> result_type;
};

template <class E1, class T2>
typename vector_binary_scalar2_traits <E1, T2, scalar_multiplies <typename E1::value_type, T2> >::result_type
operator * (vector_expression <E1>, T2)
{
}

template <class SC> struct vector_assign_traits
{
  typedef SC storage_category;
};

template <template <class, class> class F, class V, class E> void
indexing_vector_assign (V & v, vector_expression <E>)
{
  typedef F <typename V::reference, typename E::value_type> functor_type;
  typedef typename V::size_type size_type;
  size_type size (v.size ());
  for (size_type i; i <size; ++i)
    functor_type::apply (v (i), (i));
}

template <template <class, class> class F, class V, class E> void
vector_assign (V & v, const vector_expression <E> &e, dense_proxy_tag)
{
  indexing_vector_assign <F> (v, e);
}

template <template <class, class> class F, class V, class E> void
vector_assign (V & v, const vector_expression <E> &e)
{
  typedef typename vector_assign_traits <typename V::storage_category>::storage_category storage_category;
  vector_assign <F> (v, e, storage_category ());
}

template <class T, class A> struct vector : public vector_container <vector <T> >
{
  typedef vector <T> self_type;
  typedef typename A::size_type size_type;
  typedef T value_type;
  typedef typename type_traits <T>::const_reference const_reference;
  typedef T &reference;
  typedef A array_type;
  typedef vector_reference <const self_type> const_closure_type;
  typedef dense_proxy_tag storage_category;
  vector (size_type size):vector_container <self_type> (), data_ (size)
  {
  }
  vector (size_type size, value_type):vector_container <self_type> (), data_ (size)
  {
  }
  template <class AE> vector (const vector_expression <AE> &ae) : vector_container <self_type> (), data_ (ae ().size ())
  {
    vector_assign <scalar_assign> (*this, ae);
  }
  size_type
  size () const
  {
    return data_.size ();
  }
  array_type &
  data ()
  {
    return data_;
  }
  reference
  operator () (size_type i)
  {
    return data ()[i];
  }
  template <class AE> vector operator += (const vector_expression <AE> &ae)
  {
    self_type temporary (*this + ae);
    data_.swap (temporary.data ());
    return *this;
  }
  class const_iterator : public random_access_iterator_base <dense_proxy_tag>
  {
  };
  array_type data_;
};

template <class T> struct scalar_vector : public vector_container <scalar_vector <> >
{
  typedef scalar_vector self_type;
  typedef __SIZE_TYPE__ size_type;
  typedef T value_type;
  typedef T const_reference;
  typedef vector_reference <self_type> const_closure_type;
};

void
bar (vector <double>)
{
}

void
foo (int n_samp)
{
  vector <double> xi (n_samp, 0);
  for (int n = 0; n <n_samp; ++n)
    {
      vector <double> cos_w_n (n_samp);
      xi += cos_w_n * 6.0;
    }
  vector <double> cos_wd (n_samp);
  xi += cos_wd;
  bar (xi + scalar_vector <> ());
}
