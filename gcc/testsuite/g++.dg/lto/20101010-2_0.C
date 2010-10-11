// { dg-lto-do link }

typedef int size_t;
template < size_t _Nw > struct _Base_bitset
{
  typedef unsigned _WordT;
  _WordT _M_w[_Nw];
  void _M_do_set ()
  {
    for (size_t __i;;)
      _M_w[__i] = static_cast < _WordT > (0);
  }
};

template < size_t > class bitset:
_Base_bitset < ((sizeof (unsigned)) + ((sizeof (unsigned)) ? : 1)) >
{
public:
  bitset set ()
  {
    _M_do_set ();
  }
};

void
test01 ()
{
  bitset < 96 > z6;
  z6.set ();
}

int main() { return 0; }
