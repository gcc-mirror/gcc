/* PR c++/6037
   This testcase ICEd because start_enum expected pushtag to insert
   the tag always into current binding level.  */

struct A
{
  ~A () { }
};

struct B
{
  void foo ()
  {
    switch (0) { default: ; }
    A a;
    enum C { };
    (void) a;
  }
};
