// { dg-do assemble  }

template <class T, class Allocator>
  struct __vector_alloc_base 
{ 
  typedef int allocator_type; 
};

template <class T>
  struct vector :  __vector_alloc_base<T,int> 
{
  typedef short allocator_type;
  explicit vector(const allocator_type& a = allocator_type()) {}
};
