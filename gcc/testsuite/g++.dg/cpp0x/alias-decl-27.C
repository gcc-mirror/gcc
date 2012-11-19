// Origin: PR c++/54875
// { dg-do compile { target c++11 } }

template<typename T>
using AddConst = T const;

enum FwdEnum : int;

int main() {
  AddConst<FwdEnum> *ptr = nullptr;
}
