class C1 {
public:    virtual ~C1() {
}
};
class C2 : public C1 {
  C2(void *q);
  virtual void A();
};
void C2::A() {
}
C2::C2(void *q)
{
}
