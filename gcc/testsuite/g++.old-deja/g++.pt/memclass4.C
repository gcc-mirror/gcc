// { dg-do run  }
#include <typeinfo>

template <class T>
struct allocator {
  typedef T*        pointer;

  template <class U> struct rebind {
    typedef allocator<U> other;
  };
};

template <class T, class Allocator>
struct alloc_traits
{
  typedef typename Allocator::template rebind<T>::other allocator_type;
};

int main ()
{
  typedef alloc_traits<int, allocator<void> >::allocator_type at;

  return typeid (at) != typeid (allocator <int>);
}
