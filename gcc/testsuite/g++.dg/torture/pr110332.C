// { dg-do compile }

struct SlotIndex { int lie; };
SlotIndex si7, si8;

unsigned u9, u6;
bool b3, b4;
unsigned &value() {
  return b4 ? u6 : u9;
}
void transferValues() {
  unsigned RegIdx;
  SlotIndex End;
  RegIdx = value();
  End = b3 ? si7 : si8;
}
