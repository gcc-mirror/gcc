// { dg-do link  }
// GROUPS passed templates membertemplates
extern "C" int printf(const char*, ...);

struct S {
  template <class T>
  void operator+(T);
};


template <class T>
void S::operator+(T)
{
  printf("Hello, world.\n");
}



int main()
{
  S s;
  s + 3;
  s + s;
  s.operator+("Hi");
}
