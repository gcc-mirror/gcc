// { dg-options "-std=gnu++0x" }
// { dg-do compile }
template<typename... Args>
class tuple {};

void f_none(tuple<>) {}
void f_one(tuple<int>) {}
void f_two(tuple<int, float>) {}
void f_nested(tuple<int, tuple<double, char>, float>) { }


// { dg-final { scan-assembler "_Z6f_none5tupleIE" } }
// { dg-final { scan-assembler "_Z5f_one5tupleIiE" } }
// { dg-final { scan-assembler "_Z5f_two5tupleIifE" } }
// { dg-final { scan-assembler "_Z8f_nested5tupleIiS_IdcEfE" } }
