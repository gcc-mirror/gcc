// prms-id: 783

extern "C" void printf (char *, ...);

class C {
public:
  C() { }
  ~C() { }
};

int main(int argc, char**argv) {
  C c,d;
  c = (argc&1) ? C() : d;
  return 0;
}
