char c;

struct S {
  template <typename T>
  operator T*();

  template <typename T>
  operator T();
};

template <>
S::operator int()
{
  return 2;
}

template <>
S::operator char*()
{
  return &c;
}

int main()
{
  S s;
  int i = s;
  char* cp = s;

  if (i != 2 || cp != &c)
    return 1;
}
