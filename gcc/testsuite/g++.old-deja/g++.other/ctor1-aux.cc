// Origin: Mark Mitchell <mark@codesourcery.com>

template <class T>
struct S {
  template <class U>
  S (U);
};

int main ()
{
  S<int> (3.0);
}
