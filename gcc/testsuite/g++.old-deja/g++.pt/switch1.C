// { dg-do assemble  }
// Origin: Mark Mitchell <mark@codesourcery.com>

template <class T>
void f ()
{
  int i;
  
  switch (int i = 3) {
  }
}

template void f<int>();


