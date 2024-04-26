// A version of error60.C that first issues an unrelated error
// to cause the pretty printer prefix to get set, verifying we
// still print the source line for the "required from here"
// message correctly in that case.
// { dg-options "-fdiagnostics-show-caret" }

template <typename Foo>
struct my_pointer
{
  my_pointer (Foo *ptr) // { dg-message " initializing argument 1" }
  : m_ptr (ptr)
  {}

  Foo *m_ptr;
};

template <typename Foo>
void test (Foo val)
{
  my_pointer<Foo> ptr (val); // { dg-error "invalid conversion from 'int' to 'int\\*'" }
}

void usage ()
{
  unrelated_error; // { dg-error "not declared" }
  test<int> (42); // { dg-message " required from here" }
  /* { dg-begin-multiline-output "" }
   unrelated_error;
   ^~~~~~~~~~~~~~~
     { dg-end-multiline-output "" } */
  /* { dg-begin-multiline-output "" }
   test<int> (42);
   ~~~~~~~~~~^~~~
     { dg-end-multiline-output "" } */
}

  /* { dg-begin-multiline-output "" }
   my_pointer (Foo *ptr)
               ~~~~~^~~
     { dg-end-multiline-output "" } */
  /* { dg-begin-multiline-output "" }
   my_pointer<Foo> ptr (val);
                        ^~~
                        |
                        int
     { dg-end-multiline-output "" } */
