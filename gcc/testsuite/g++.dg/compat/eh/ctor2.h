struct VBase
{
  virtual void f () {}
  VBase();
  ~VBase();
};

struct  StreamBase 
{
  virtual ~StreamBase() {}
};

struct  Stream : public virtual VBase, public StreamBase
{
  Stream();
  virtual ~Stream() {} 
};

struct DerivedStream : public Stream
{
  DerivedStream();
};
