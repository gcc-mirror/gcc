// PR c++/62302

// { dg-do compile { target *-*-*gnu* } }
// { dg-final { scan-assembler "_ZN3optIiED5Ev,comdat" } }
// { dg-final { scan-assembler-not "_ZN3optIiED0Ev,comdat" } }
// { dg-final { scan-assembler-not "_ZN3optIiED1Ev,comdat" } }
// { dg-final { scan-assembler-not "_ZN3optIiED2Ev,comdat" } }

struct Option {
  virtual ~Option() {}
};
template <class DataType> class opt : public Option {};
template class opt<int>;
