// { dg-do link  }
// GROUPS passed templates
template <class T>
void foo(T);

class S {
  friend void foo<>(int);

  int i;
};


template <>
void foo(int)
{
  S s;
  s.i = 3;
}


int main()
{
  foo(3);
}
