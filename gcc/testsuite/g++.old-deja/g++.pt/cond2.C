// Build don't link:
// Origin: Mark Mitchell <mark@codesourcery.com>

struct S
{
  S (int);
  operator bool () const;
};

template <class T>
void f ()
{
  if (const S &s = 3) {
  }
}

template void f<int>();

