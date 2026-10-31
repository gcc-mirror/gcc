// { dg-lto-do link }
// { dg-require-effective-target c++11 }

// Check that the type decltype(nullptr) is pretty printed when LTO is involved

decltype (nullptr) x; // { dg-lto-message "type 'decltype\\(nullptr\\)' should match type 'int'" }

int get (void);

int
main ()
{
  return get ();
}
