
template<typename _Tp>
class allocator {};

template<typename _Alloc> struct allocator_traits;

template<typename _Tp>
struct allocator_traits <allocator<_Tp>>
{
  using pointer = _Tp*;
};

struct mutex {};

template<typename _Tp, typename _Alloc>
class Inplace
{
public:
  virtual void _M_dispose() noexcept
  {
    // bogus error ambiguous partial specializations
    typename allocator_traits<_Alloc>::pointer v;
  }
};

inline void *
allocate_shared()
{
  return new Inplace<mutex, allocator<mutex>> ();
}
