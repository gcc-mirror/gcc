// { dg-do assemble  }

struct S0 { virtual void f1 () { } };

struct S1 : virtual public S0 { virtual void f1 () { } };

struct S2 : public S1 { virtual void f1 () { } };

struct S3 : virtual public S0 { virtual void f1 () { } };

struct S4 : public S3 { };

void creator () { new S4; }
