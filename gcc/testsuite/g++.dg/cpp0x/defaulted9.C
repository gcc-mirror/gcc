// PR c++/39153

struct _Impl_base
{
  _Impl_base() = default;
  virtual ~_Impl_base() = default;
};

template<typename _Tp>
class _Impl : public _Impl_base
{ };

int main()
{
  _Impl<int> i;
  return 0;
}
