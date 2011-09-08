/*  Origin PR c++/33255
    { dg-options "-Wunused-local-typedefs" }
    { dg-do compile }
*/

void
test_warn ()
{
  typedef int foo; // { dg-warning "locally defined but not used" }
}

void
test0 ()
{
    typedef int foo;
    foo var __attribute__((unused));
}

void
test1 ()
{
    typedef int foo;
    const foo *var = 0;
}

void
test2 ()
{
  typedef int foo;
  void func(foo);  
}

void
test7 (void)
{
  typedef int foo;
  int vec[1] = {sizeof (foo)};
}

void
test8 (void)
{
  typedef int foo __attribute__((used));
}
