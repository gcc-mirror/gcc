
template <typename _Tp>
struct Base
{
  // template constructor
  template<typename _Del> Base(_Tp *__p, _Del __d);
};

template <typename _Tp, typename _Dp>
struct Derived : Base<_Tp>
{
  // Inheriting the template constructor
  using Base<_Tp>::Base;
};

template <typename _Tp>
class unique_ptr
{
  Derived<_Tp, int> _M_t;

public:
  // Instantiates Derived<ResultDerived,int>::Derived
  template<typename _Up> unique_ptr(unique_ptr<_Up>&& __u) noexcept
    : _M_t ((_Tp *)0, 1) { }
};

struct ResultBase { };
struct ResultDerived : ResultBase { };

void Frob (unique_ptr<ResultBase> &&__res) ;

inline void X (unique_ptr<ResultDerived> &parm)
{
  Frob (static_cast <unique_ptr<ResultDerived> &&> (parm));
}
