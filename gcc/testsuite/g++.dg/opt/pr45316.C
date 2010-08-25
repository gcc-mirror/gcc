// { dg-do compile }
// { dg-options "-O1 -ftree-pre -fnon-call-exceptions" }

struct A
{
  int i;
};

struct B : A
{
  int i[6];
  B (int = 0) : A ()
  {
    m ();
  }
  int m ();
};

struct C : B
{
};

void
foo ()
{
  new C ();
}

