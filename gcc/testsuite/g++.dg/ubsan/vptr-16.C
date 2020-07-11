// PR c++/95311
// { dg-additional-options -fsanitize=undefined }

class a {
  virtual long b() const;
};
class c : a {
public:
  long b() const;
};
class d : c {
  long e();
};
long d::e() { b(); return 0; }
