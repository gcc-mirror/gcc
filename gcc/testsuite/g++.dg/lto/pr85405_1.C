class VclReferenceBase {
  int mnRefCnt;
  bool mbDisposed;

protected:
  virtual ~VclReferenceBase();
};
class : VclReferenceBase {
} a;
