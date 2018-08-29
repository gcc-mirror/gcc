class VclReferenceBase {
  int mnRefCnt;
  int mbDisposed: 7; // { dg-lto-message "19: a field of same name but different type is defined in another translation unit" }

protected:
  virtual ~VclReferenceBase();
};
class : VclReferenceBase {
} a;
