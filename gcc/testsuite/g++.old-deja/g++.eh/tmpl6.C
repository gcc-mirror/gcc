// Build don't link:
// Origin: Mark Mitchell <mark@codesourcery.com>

struct S
{
  int i;
};

template <class T>
void f ()
{
  try {
  } catch (S& s) {
    s.i = 3;
  }
}

template void f<int>();
