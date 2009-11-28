// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin: PR c++/36408
// { dg-options "" }
// { dg-do compile }

template<int>
void
foo()
{
  int i = ({ }); // { dg-error "void value not ignored" }
}

template<int>
void
bar()
{
  int i = ({ ({}); }); // { dg-error "void value not ignored" }
}

int
main ()
{
  foo<0> ();
  bar<0> ();
}

