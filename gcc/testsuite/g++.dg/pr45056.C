/* { dg-do compile } */
/* { dg-options "-O -fschedule-insns2 -fschedule-insns -g" } */

template < class _T1, class _T2 > struct pair
{
  _T1 first;
  _T2 second;
  pair (_T1 & __a, _T2 & __b)
      :first (__a), second (__b)   
    {    }
}
;
template < typename _Tp > struct _Vector_base
{
  ~_Vector_base ();
}
;
template < typename _Tp>struct vector
: _Vector_base < _Tp>
{
  template    <    typename     _ForwardIterator >   inline     void _Destroy (_ForwardIterator)   {  }
  _Tp * _M_finish;
  ~vector ()
    {
      _Destroy 	( this->_M_finish);
    }
}   ;
template < typename ITV > struct Box
{
  Box (const Box &);
  Box ();
  typedef vector < ITV > Sequence;
  Sequence seq;
};
template < typename D > struct Powerset
{
  Powerset (const Powerset &y) :reduced (y.reduced) {}
  bool reduced;
} ;
template < typename PS > struct Pointset_Powerset :Powerset <  int >
{
  Pointset_Powerset ();
  int space_dim;
} ;
pair
<
Box<int>,
Pointset_Powerset < int > >
linear_partition ()
{
  Pointset_Powerset < int > r ;
  Box<int> qq;
  return pair<Box<int>,Pointset_Powerset < int > >  (qq, r);
}
