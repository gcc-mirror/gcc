// { dg-do compile }
// { dg-options "-fgnu-tm -O3" }

template<typename _InputIterator, typename _Distance>  inline void advance(_InputIterator& __i, _Distance __n)
  {
    if (__n > 0) 
      while (__n--)
        --__i;
    else  
      --__i;
  }

void  _Rb_tree_increment ();

template<typename _Tp> struct _Rb_tree_iterator 
  {
    typedef _Rb_tree_iterator<_Tp> iterator;

    iterator& operator--()
      {
        _Rb_tree_increment();
      }
  };

void update ()
  {
    _Rb_tree_iterator<int>::iterator it;
    __transaction_relaxed
      {
        advance (it, 0);
      }
  }

