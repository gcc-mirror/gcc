// PR c++/52374
// { dg-do compile { target c++11 } }

struct B
{
  int get() const { return 42; }
};

template<typename X>
struct D
  : public X
{
  int get() const { return [this]() -> int { return X::get(); }(); }
};

int main()
{
  D<B> d;
  d.get();
}
