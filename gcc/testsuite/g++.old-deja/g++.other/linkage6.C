// Build don't link:
// Special g++ Options: -fno-weak
// Origin: Mark Mitchell <mark@codesourcery.com>

template <typename T>
void f ();

void h () { f<int> (); }

template <void (*X)()>
void g () {}

template <typename T>
void f ()
{
   g<&f<T> >();
}

