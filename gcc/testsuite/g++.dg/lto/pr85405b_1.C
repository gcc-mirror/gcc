class VclReferenceBase {
  int mnRefCnt;
  int mbDisposed: 7; 

protected:
  virtual ~VclReferenceBase();
};
class : VclReferenceBase {
} a;
