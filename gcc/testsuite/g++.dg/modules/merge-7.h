template<typename _Tp2, typename _Up2>
struct __promote_2
{
  typedef __typeof__(_Tp2() + _Up2()) __type;
};
