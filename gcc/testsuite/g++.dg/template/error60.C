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
  test<int> (42); // { dg-message " required from here" }
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
