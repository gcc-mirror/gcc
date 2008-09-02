// PR tree-optimization/36766
// { dg-do compile }
// { dg-options "-O -fnon-call-exceptions" }

struct A
{
  ~A ()
  {
    int *a = this->b;
  }
  int *b;
};

struct B : A
{
  B ()
  {
    int *a = this->b;
  }
   ~B ()
  {
    int *a = this->b;
  }
};

void
foo ()
{
  B *c = new B;
  delete c;
}
