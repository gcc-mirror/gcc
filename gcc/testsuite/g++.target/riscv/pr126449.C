/* PR target/126449 */
/* { dg-do compile { target { rv64 } } } */
/* { dg-options "-mcmodel=large -fno-pie" } */

struct AddIn
{
  virtual ~AddIn ();
};

struct Base
{
  virtual ~Base ();
};

struct Deriv : Base, AddIn
{
};

int
main ()
{
  Deriv deriv;
}
