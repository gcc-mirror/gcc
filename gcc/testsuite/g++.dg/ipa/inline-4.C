// PR c++/104539
// { dg-additional-options "-O3 -fdump-ipa-inline" }
// { dg-final { scan-ipa-dump-not "overwritten at link time" "inline" } }

template <int>
//inline
int f() {
  return 0;
}

template int f<0>();

int g() {
  return f<0>() + 1;
}
