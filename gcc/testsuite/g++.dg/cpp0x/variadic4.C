// { dg-options "-std=gnu++0x -fabi-version=0" }
// { dg-do compile }
template<typename... Args>
class tuple {};

void f_none(tuple<>) {}
void f_one(tuple<int>) {}
void f_two(tuple<int, float>) {}
void f_nested(tuple<int, tuple<double, char>, float>) { }


// { dg-final { scan-assembler "_Z6f_none5tupleIJEE" } }
// { dg-final { scan-assembler "_Z5f_one5tupleIJiEE" } }
// { dg-final { scan-assembler "_Z5f_two5tupleIJifEE" } }
// { dg-final { scan-assembler "_Z8f_nested5tupleIJiS_IJdcEEfEE" } }
