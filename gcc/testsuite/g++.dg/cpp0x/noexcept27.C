// PR c++/55443
// { dg-do compile { target c++11 } }

struct X
{
  X() = default;

  X(int);

  void* operator new(__SIZE_TYPE__, void*) noexcept;
};

X x;

bool b = noexcept(new(0) X);
