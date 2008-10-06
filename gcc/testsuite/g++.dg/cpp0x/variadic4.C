// { dg-options "-std=gnu++0x" }
// { dg-do compile }
template<typename... Args>
class tuple {};

void f_none(tuple<>) {}
void f_one(tuple<int>) {}
void f_two(tuple<int, float>) {}
void f_nested(tuple<int, tuple<double, char>, float>) { }


// { dg-final { scan-assembler "_Z6f_none5tupleIIEE" } }
// { dg-final { scan-assembler "_Z5f_one5tupleIIiEE" } }
// { dg-final { scan-assembler "_Z5f_two5tupleIIifEE" } }
// { dg-final { scan-assembler "_Z8f_nested5tupleIIiS_IIdcEEfEE" } }
