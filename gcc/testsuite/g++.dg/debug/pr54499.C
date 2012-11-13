// PR debug/54499
// { dg-do assemble }

struct S1
{
  virtual void f () = 0;
};

struct S2
{
  virtual ~S2 () { }
};

struct S3 : public S1, public S2
{
  void f ();
};

void
S3::f ()
{
}
