namespace X{
  void f(int){}
}

void f();

int main()
{
  using X::f;
  f(3);
}
