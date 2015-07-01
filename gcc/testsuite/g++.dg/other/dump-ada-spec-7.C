/* { dg-do compile } */
/* { dg-options "-fdump-ada-spec" } */

template<int n>
void bar ()
{
  return;
}

class Foo
{
  // This check that we properly skip the specification for templated
  // members of non-templated classes.
  template<int n>
  void bar ();
};

template<int n>
void Foo::bar ()
{
  return;
}

/* { dg-final { cleanup-ada-spec } } */
