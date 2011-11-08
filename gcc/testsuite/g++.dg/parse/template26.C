// PR c++/50864

namespace impl
{
  template <class T> T create();
}

template <class T, class U, __SIZE_TYPE__
	  = sizeof(impl::create<T>()->*impl::create<U>())>
struct foo1;

template <class T, class U, __SIZE_TYPE__
	  = sizeof(impl::create<T>()->impl::create<U>())> // { dg-error "not a class member" }
struct foo2;

template <class T, class U, __SIZE_TYPE__
	  = sizeof(impl::create<T>().impl::create<U>())> // { dg-error "not a class member" }
struct foo3;
