// { dg-do assemble  }
// Origin: Mark Mitchell <mark@codesourcery.com>

template <class T>
void h(T&);

template <class T>
void g () 
{
  h ("abcdefghi");
}

template void g<int>();

template <class T>
void h(T&)
{
  T t = {};
}


