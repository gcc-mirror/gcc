// PR c++/4460
// Test that the cleanup for fully-constructed subobjects when a
// constructor throws gets the right address for a virtual base.

// { dg-do run }

int r;
void *p;

struct VBase
{
  virtual void f () {}
  VBase() { p = this; }
  ~VBase() { if (p != this) r = 1; }
};

struct  StreamBase 
{
  virtual ~StreamBase() {}
};

struct  Stream : public virtual VBase, public StreamBase
{
  Stream() {}
  virtual ~Stream() {} 
};

struct DerivedStream : public Stream
{
  DerivedStream() { throw 1; }
};

int main() {

  try
    { 
      DerivedStream str;
    }
  catch (...) { }

  return r;
}
