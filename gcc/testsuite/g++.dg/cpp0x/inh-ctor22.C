// { dg-do compile { target c++11 } }

class A { };
template<typename> using UniquePtr = int;
template<typename AllocPolicy> struct BufferList {
  BufferList(unsigned, unsigned, unsigned, AllocPolicy = AllocPolicy());
};
class D : BufferList<A> {
  using BufferList::BufferList;
};
template<typename , typename... Args> UniquePtr<D> MakeUnique(Args... aArgs)
{
  D d(aArgs...);
  return 0;
}
UniquePtr<D> setCloneBuffer_impl_buf = MakeUnique<D>(0, 0, 0);
