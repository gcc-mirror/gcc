// Build don't link:
// Origin: Mark Mitchell <mark@codesourcery.com>

class ostream;

extern ostream& cout;

class Foo { };

ostream &operator<<(ostream &os, const Foo &)
{
  return os;
}

template<class T>
const typename T::fooz &operator<<(const Foo &, const T &t)
{
  return t;
}

int main()
{
  Foo foo;

  cout << foo;
}
