// { dg-do run }
// { dg-options "-O2" }

extern "C" int printf (const char*, ...);

struct _Deque_iterator {
  int _M_cur;
  int x[2];
  int* _M_node;

  _Deque_iterator() : _M_cur(0), _M_node(0) {}
  _Deque_iterator(const _Deque_iterator& __x)
    : _M_cur(__x._M_cur),
      _M_node(__x._M_node) {}
};

class _Deque_base
{
public:
  int yy;

  _Deque_base()
    : _M_start()
    { _M_initialize_map(); }
  ~_Deque_base();   

  void _M_initialize_map();
  _Deque_iterator _M_start;
};


_Deque_base::~_Deque_base() {
  printf ("bb %x %x\n", this, *_M_start._M_node);
}

void
_Deque_base::_M_initialize_map()
{
  yy = 0x123;
  printf ("aa %x %x\n", this, yy);

  _M_start._M_node = &yy;
  _M_start._M_cur = yy;
}


class deque : protected _Deque_base
{
public:
  deque () {}
  deque(const deque& __x) {}
  ~deque() {
    _Deque_iterator i = _M_start;
  }
};



class GeometryAddress {
public:
  GeometryAddress(deque addressStack) {}
};

void yyy (const GeometryAddress& gb)
{
}

deque temp1;

int main()
{
  yyy (GeometryAddress (temp1));
  return 0;
}
