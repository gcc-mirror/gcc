// PR c++/46348
// { dg-do compile { target c++11 } }

template<__SIZE_TYPE__ _Nw>
  struct _Base
  {
    typedef unsigned long _WordT;

    _WordT _M_w[_Nw];

    constexpr
    _Base()
    : _M_w() { }
  };

int main()
{
  _Base<256> bs;
}
