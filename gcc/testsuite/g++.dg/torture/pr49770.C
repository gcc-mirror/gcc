/* { dg-do run } */
/* { dg-options "-std=c++0x -fno-tree-forwprop" } */

typedef __SIZE_TYPE__ size_t;

template < typename > struct remove_reference;
template < typename _Tp > struct remove_reference <_Tp & >
{
  typedef _Tp type;
};
template < typename _Tp > typename remove_reference < _Tp >::type &&
move (_Tp && __t)
{
  return static_cast < typename remove_reference < _Tp >::type && >(__t);
}

template < typename _Tp > void
stdswap (_Tp & __a, _Tp & __b)
{
  _Tp __tmp (__a);
  __a = (__b);
  __b = (__tmp);
}

struct _Deque_iterator
{
  int *_M_cur;
  int *_M_first;
  int *_M_last;
  int **_M_node;
};

static inline int operatorMIN (_Deque_iterator & __x, _Deque_iterator & __y)
{
  return sizeof (int) * (__x._M_node - __y._M_node - 1)
    + (__x._M_cur - __x._M_first) + (__y._M_last - __y._M_cur);
}

struct deque
{
  deque & operator = (deque && __x)
  {
    stdswap (_M_finish, __x._M_finish);
    return *this;
  }
  size_t size ()
  {
    return operatorMIN (_M_finish, _M_start);
  }

deque ():
  _M_map (), _M_map_size (), _M_start (), _M_finish ()
  {
    _M_start._M_last = _M_start._M_first + sizeof (int);
  }

  int **_M_map;
  size_t _M_map_size;
  _Deque_iterator _M_start;
  _Deque_iterator _M_finish;
};

struct queue
{
  deque c;
  size_t size ()
  {
    return c.size ();
  }
};

void
test01 ()
{
  queue a, b;
  ++a.c._M_finish._M_cur;
  b = move (a);
  if (!b.size ())
    __builtin_abort ();
}

int
main ()
{
  test01 ();
  return 0;
}

