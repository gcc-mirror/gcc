// { dg-final { scan-assembler "_Z3fi1B6_X_tagv" } }

struct __attribute((abi_tag("_A1_tag"))) A1 {};
template <class T> struct __attribute((abi_tag("_X_tag"))) X {};
X<int> fi1();
int main() {
  X<A1> xa;
  fi1();
}
