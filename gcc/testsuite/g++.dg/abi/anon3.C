// { dg-require-weak "" }

typedef struct {
  // { dg-final { scan-assembler ".weak\(_definition\)?\[ \t\]_?_ZN4Heya4blahEv" } }
  // { dg-final { scan-assembler ".weak\(_definition\)?\[ \t\]_?_ZTI4Heya" { target { ! { hppa*-*-hpux* } } } } }
  // { dg-final { scan-assembler ".weak\(_definition\)?\[ \t\]_?_ZTV4Heya" } }
  virtual const char *blah() {
    return "Heya::blah";
  }
  // { dg-final { scan-assembler ".weak\(_definition\)?\[ \t\]_?_ZN4Heya1A1fEv" } }
  // { dg-final { scan-assembler ".weak\(_definition\)?\[ \t\]_?_ZTIN4Heya1AE" { target { ! { hppa*-*-hpux* } } } } }
  // { dg-final { scan-assembler ".weak\(_definition\)?\[ \t\]_?_ZTVN4Heya1AE" } }
  struct A {
    virtual void f() { }
  };
} Heya;

Heya h;
Heya::A a;
