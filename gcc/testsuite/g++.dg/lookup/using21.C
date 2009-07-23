// PR c++/40799

namespace Bar {
  typedef int A;
}
class CollectionDeleteGuard {
public:
  CollectionDeleteGuard(int);
};
CollectionDeleteGuard::CollectionDeleteGuard(int)
{
  using Bar::A;
}
