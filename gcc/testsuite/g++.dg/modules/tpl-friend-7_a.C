// { dg-additional-options -fmodules-ts }

export module foo;
// { dg-module-cmi foo }

template<typename _Tp>
class new_allocator
{
  template<typename _Up>
  friend bool
  operator!=(const new_allocator&, const new_allocator<_Up>&)
    noexcept
  { return false; }
};

new_allocator<char> x;
