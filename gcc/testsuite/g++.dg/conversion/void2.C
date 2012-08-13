// PR c++/54165

struct A
{
  template<typename T>
  operator T()
  {
    T l[];
  }
};

int main()
{
  A a;
  (void)a;
}
