// { dg-do assemble  }
// Origin: Mark Mitchell <mark@codesourcery.com>

char foo[26];

template <class T>
void f ()
{
  foo = "0123456789012345678901234"; // { dg-error "array" }
}

template void f<int>();
