// Build don't link:
// Special g++ Options: -fpermissive -w
// Origin: Mark Mitchell <mark@codesourcery.com>

char foo[26];

template <class T>
void f ()
{
  foo = "0123456789012345678901234";  
}

template void f<int>();
