// { dg-do compile }
// { dg-options "-ftracer -fno-tree-dce -fno-tree-sra -Wno-return-type" }

struct bidirectional_iterator_tag
{};
struct random_access_iterator_tag:bidirectional_iterator_tag
{};
template < typename _Category, typename, typename _Distance, typename > struct iterator
{
  typedef _Distance difference_type;
};
template < typename _Iterator > struct iterator_traits
{
  typedef typename _Iterator::difference_type difference_type;
};
template < typename _Tp > struct iterator_traits <_Tp * >
{
  typedef random_access_iterator_tag iterator_category;
  typedef _Tp value_type;
  typedef int difference_type;
  typedef _Tp reference;
};
template < typename _Iterator > class reverse_iterator:
    public
    iterator < typename iterator_traits < _Iterator >::iterator_category,
    typename iterator_traits < _Iterator >::value_type,
    typename iterator_traits < _Iterator >::difference_type, typename iterator_traits < _Iterator >::reference >
{
  _Iterator current;
public:
  typedef _Iterator iterator_type;
  reverse_iterator (const reverse_iterator & __x):current (__x.current)
  {}
  iterator_type base ()
    {
      return current;
    }
  reverse_iterator operator++ ()
    {
      --current;
    }
};
template
<
typename
_Iterator
>
bool
operator
==
(reverse_iterator < _Iterator > __x, reverse_iterator < _Iterator > __y)
{
  return __x.base () == __y.base ();
}

template
<
typename
_Iterator
>
typename
reverse_iterator
<
_Iterator
>::difference_type
operator
- (reverse_iterator < _Iterator >, reverse_iterator < _Iterator >)
{}
template
<
typename
_RandomAccessIterator
>
_RandomAccessIterator
__find
(_RandomAccessIterator
 __first, _RandomAccessIterator __last)
{
  typename
      iterator_traits
      <
      _RandomAccessIterator
      >::difference_type __trip_count (__last - __first);
  for (; __trip_count; --__trip_count)
    ++__first;
  return __last;
}
typedef reverse_iterator < int* > _ForwardIterator1;
_ForwardIterator1
search
(_ForwardIterator1
 __first1,
 _ForwardIterator1
 __last1)
{
  for (;;)
    {
      __first1 = __find (__first1, __last1);
      if (__first1 == __last1)
	return __last1;
    }
}
