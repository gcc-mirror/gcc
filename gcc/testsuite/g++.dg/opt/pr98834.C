/* { dg-do compile } */
/* { dg-require-effective-target c++17 } */
/* { dg-options "-O2 -fdump-tree-fre3" } */

struct _Base
{ 
  int _M_data = 0;
};

struct _Wrapper : _Base
{ 
  _Wrapper(int) {}

  bool _M_is_constprop() { return __builtin_constant_p(_M_data); }
};

struct _Impl
{ 
  _Wrapper _S_multiplies(_Wrapper __x, _Wrapper __y)
  { 
    if (__x._M_is_constprop() || __y._M_is_constprop())
      return __y;
    return 0;
  }
};

struct _TupleData
{ 
  _Wrapper first;
  int second;
};

struct _Tuple : _TupleData
{ 
  template <typename _Fp>
    _Tuple _M_apply_per_chunk(_Fp __fun, _Tuple __y)
    { 
      return {__fun(first, __y.first), second};
    }
};

struct _ImplFixed
{ 
  static _Tuple _S_multiplies(_Tuple __x, _Tuple __y)
  { 
    return __x._M_apply_per_chunk(
             []( auto __xx, auto __yy) {
               return _Impl()._S_multiplies(__xx, __yy);
             },
             __y);
  }
};

class simd
{
public:
  [[__gnu__::__always_inline__]] friend simd operator*(simd __x, simd __y)
  { return _ImplFixed::_S_multiplies(__x._M_data, __y._M_data); }

  simd(_Tuple __init) : _M_data(__init) {}

  _Tuple _M_data;
};

int main()
{ 
  simd({0, 0}) * simd({0, 0});
}

/* FRE3 should elide all conditionals in the remaining main.  */
/* { dg-final { scan-tree-dump-times "<bb" 1 "fre3" } } */
