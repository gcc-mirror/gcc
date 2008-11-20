// PR c++/37540

struct A
{
  int g() {return 0;}
};

template <typename T_> 
void f(A a)
{
  __decltype(a.g()) i;
}

int main()
{
  f<int>(A());
}
