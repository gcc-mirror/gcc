// PR c++/18466

int ::i; // { dg-error "" }
void ::f(); // { dg-error "" }
namespace N {
  int N::j; // { dg-error "" }
  void N::g(); // { dg-error "" }
}
