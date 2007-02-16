/* { dg-do compile } " */
/* { dg-options "" } */

void foo(int i)
{
  (i ? 1 : 2) = ({ X; }); /* { dg-error "" } */
}

struct A
{
  ~A ();
  void foo()
  {
    delete this = ({ X; }); /* { dg-error "" } */
  }
};
