// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin: PR c++/36408
// { dg-options "" }
// { dg-do compile }

template<int>
void
foo()
{
  int i = ({ }); // { dg-error "cannot convert" }
}

template<int>
void
bar()
{
  int i = ({ ({}); }); // { dg-error "cannot convert" }
}

int
main ()
{
  foo<0> ();
  bar<0> ();
}

