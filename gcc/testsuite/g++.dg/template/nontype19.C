// PR c++/42466

template<class IntT, IntT X>
struct A
{
  A();

  template<IntT X2>
  A(const A<IntT, X2>& other);
};

int main(int argc, char** argv)
{
    A<int, 42> a;
    A<int, 100> b = a;

    A<unsigned, 42u> c;
    A<unsigned, 100u> d = c;
}
