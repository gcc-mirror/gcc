// PR tree-optimization/39259
// { dg-do compile }
// { dg-options "-O2" }


extern "C" int __mysetjmp () __attribute__ ((__returns_twice__));

class TContStatus {};

class TContEvent
{
public:
  inline void Execute () throw();
};

class TCont
{
public:
  TContStatus ReadD (void* buf, int deadline)
  {
    TContEvent event;
    event.Execute ();
    return TContStatus();
  }
  TContStatus ReadI (void *buf)
  {
    return ReadD (buf, 1);
  }
};

void TContEvent::Execute () throw ()
{
  __mysetjmp();
}

void Broken (TCont *mCont)
{
  mCont->ReadI(0);
  mCont->ReadI(0);
}
