// PR c++/29633

template <typename T>
struct Class1
{
  void testfn1(void);
};

template <typename T>
class Class2
{
public:
  void testfn2(void)
  {
    Class1<T> * tc_a;
    do
    {
      int x = 0;
    }
    while (tc_a && tc_a->testfn1);  // { dg-error "invalid use of member" }
  }
};

int main(void)
{
  Class2<int> tc2;
  tc2.testfn2();
  return 0;
}
