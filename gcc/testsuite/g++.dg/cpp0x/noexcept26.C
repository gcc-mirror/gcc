// PR c++/55443
// { dg-do compile { target c++11 } }

struct X
{
  constexpr X() { }

  void* operator new(__SIZE_TYPE__, void*) noexcept;
};

int main() { noexcept(new(0) X); }
