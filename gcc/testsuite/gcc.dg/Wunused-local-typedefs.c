/*  Origin PR c/53702
    { dg-options "-Wunused-local-typedefs" }
    { dg-do compile }
*/

/* Only test nested functions for C.  More tests that work for C and C++
   can be found in c-c++-common.
*/

void
test0 ()
{
  typedef int foo; /* { dg-warning "locally defined but not used" } */
  void f ()
  {
  }
}

void
test1 ()
{
  void f ()
  {
    typedef int foo; /* { dg-warning "locally defined but not used" } */
  }
}


void
test2 ()
{
  void f ()
  {
  }
  typedef int foo; /* { dg-warning "locally defined but not used" } */
}
