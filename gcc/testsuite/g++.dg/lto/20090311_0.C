class C1 {
public:     virtual ~C1() {
}
};
class C2 : public C1 {
public:
  C2(void *q);
  virtual void A();
};
int main(int argc, char **argv) {
  C2 h(0);
  return 0;
}
