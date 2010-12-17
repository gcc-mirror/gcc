// { dg-do compile }

struct c1 {};

struct c10 : c1
{
  virtual void foo ();
};

struct c11 : c10, c1		//  // { dg-warning "" }
{
  virtual void f6 ();
};

struct c28 : virtual c11
{
  void f6 ();
};

void check_c28 ()
{
  c28 obj;
  c11 *ptr = &obj;
  ptr->f6 ();
}
