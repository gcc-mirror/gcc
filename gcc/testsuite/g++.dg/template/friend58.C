// PR c++/42328

template<typename T, typename U>
class freeList
{
public:
  void foo() {};
};

class bar {};

class baz : protected freeList<bar, baz>
{
  template<typename T>
  friend
  void freeList<T, baz>::foo();  // { dg-error "friend" }
};

baz b;
