// Build don't link:
// Origin: Mark Mitchell <mark@codesourcery.com>

int i;

struct S
{
  ~S () 
  {
  }
};

template <class T>
void f (T, S)
{
  i = 0;
}

int main ()
{
  i = 1;
  f (3, S ());
  return i;
}
