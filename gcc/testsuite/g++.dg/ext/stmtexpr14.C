// PR c++/51714

template <typename T>
void Foo()
{
  true || !__extension__ ({ int verbose = 2; verbose <= 3; });
}

int main()
{
  Foo<int>();
}
