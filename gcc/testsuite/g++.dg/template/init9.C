// PR c++/20140

template<typename T> void foo()
{
  unsigned char s[] = "";
}

int main()
{
  foo<char>();
  foo<int>();
}
