// Build don't link:
// Origin: Mark Mitchell <mark@codesourcery.com>

template <class T>
void f ()
{
  extern int i;
  extern T j;
  
  i = j;
}

template void f<int>();
