/* { dg-do compile } */
/* { dg-options "-O1 -fipa-sra -fnon-call-exceptions" } */

struct A
{
  ~A () { }
};

struct B
{
  A a;
  int i;
  void f (int) { }
  B ()
  {
    f (i);
  }
};

B b;
