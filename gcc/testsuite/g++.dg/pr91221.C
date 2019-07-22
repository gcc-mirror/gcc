// { dg-do compile }
// { dg-options "-O2 -fno-ipa-pure-const -fpack-struct -Wno-address-of-packed-member" }

void printf(...);
struct A {
    A() : bar_(), dbar_() {
	for (int i;; i++)
	  printf(i, bar_[i]);
    }
    int bar_[5];
    double dbar_[5];
};
void fn1() { A a; }
