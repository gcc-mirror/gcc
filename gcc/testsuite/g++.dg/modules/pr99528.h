template<typename _Tp>
class new_allocator
{
public:
  template<typename _Tp1>
  new_allocator(const new_allocator<_Tp1>&) noexcept { }
};

extern template class new_allocator<char>;
