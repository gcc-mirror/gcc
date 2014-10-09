/* { dg-do compile } */
/* { dg-options "-O3" } */

/* The bug only presented itself on a 32 bit i386 but in theory it might also
   pop up elsewhere and we do not want to put -m32 options to testcase
   options.  */

struct A
{
  virtual int a (int, int = 0) = 0;
  void b ();
  void c ();
  int d;
};

struct B : virtual A
{
  int a (int, int);
  int e;
};

int f;

void
A::b ()
{
  a (0);
}

void
A::c ()
{
  a (f);
}

int
B::a (int, int)
{
  return e;
}
