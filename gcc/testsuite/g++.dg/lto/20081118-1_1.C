class C {
 public:
  C();
  virtual ~C();
  virtual void foo();
};
class D  {
  ~D();
  C lexer_;
};
D::~D() {
}
