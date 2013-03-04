// PR c++/10291
// { dg-do link }

template <class T>
int foo ()
{
  static int i;

  struct S {
    int bar () {
      return i;
    }
  } s;

  return s.bar ();
}

int main ()
{
  foo<int>();
}
