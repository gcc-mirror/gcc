#pragma interface
struct S
{
  S *s;
  ~S ()
  {
    delete s;
  }
};

S s;
