// { dg-do compile { target c++11 } }

struct minus {
} _1;
int _2;
struct _Bind {
  _Bind(_Bind &);
};
template <typename _Func, typename _BoundArgs>
_Bind bind(_Func, _BoundArgs &&, ...);
void test01() { bind(minus(), _2, _1); }
