// { dg-do assemble  }
// Origin: Mark Mitchell <mark@codesourcery.com>

template <class T>
void f ()
{
  __extension__ ( { if (3); });
}

template void f<int>();
