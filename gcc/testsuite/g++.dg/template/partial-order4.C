// DR 532
// PR c++/53499
// [temp.func.order] says that we do ordering on the first parameter.

struct A
{
  template <class T>
  bool operator==(T);
};

template <class T, class U>
bool operator==(T, U);

int main()
{
  A() == A();
}
