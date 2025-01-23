// { dg-additional-options "-fmodules-ts -fno-module-lazy" }

inline auto foo() {  // { dg-error "conflicting" }
  return 1.0;
}
import "auto-6_a.H";
