// Origin: PR c++/43704
// { dg-do compile }

template<typename T2, typename T3>
struct if_
{
 typedef T2 type;
};

template<class I1>
struct iterator_restrict_traits
{
    struct iterator_category {};
};

template<class T>
struct matrix
{
 struct ci {struct ic {};};
 class i {};
};

template<class M, class TRI>
struct triangular_adaptor
{
   typedef typename if_<typename M::ci,typename M::i>::type ty1;
   class iterator2 :  iterator_restrict_traits<typename ty1::ic>::iterator_category
   {
   };
};

template<class M>
struct banded_adaptor
{
  typedef typename if_<typename M::ci,typename M::i>::type ty1;
  class iterator1 :  iterator_restrict_traits<typename ty1::ic>::iterator_category
  {
  };
};

template<class T>
struct singular_decomposition
{
  banded_adaptor<matrix<double> >::iterator1 it1;
};

