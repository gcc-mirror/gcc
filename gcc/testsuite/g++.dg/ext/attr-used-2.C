// PR c++/67453
// { dg-do compile }
// { dg-final { scan-assembler "_ZN1SC\[12]Ev" } }
// { dg-final { scan-assembler "_ZN1SD\[12]Ev" } }
// { dg-final { scan-assembler "_ZN1SC\[12]ERKS_" } }

struct S {
    S();
    ~S();
    S(const S&);
};

__attribute__((used)) inline S::S()  { }
__attribute__((used)) inline S::~S() { }
__attribute__((used)) inline S::S(const S&) { }
