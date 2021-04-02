// { dg-additional-options {-std=c++2a -fmodule-header} }

import  "pr99283-6_b.H";

template<typename _Alloc>
struct __allocated_ptr
{
  using value_type = allocator_traits<_Alloc>;
};

