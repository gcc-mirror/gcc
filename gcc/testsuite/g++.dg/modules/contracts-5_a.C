// PR c++/108205
// Test that the implicitly declared handle_contract_violation function is
// properly matched with a later declaration in an importing TU.
// { dg-additional-options "-fmodules -fcontracts -fcontract-continuation-mode=on" }
// { dg-module-cmi test }

export module test;
export inline void foo(int x) noexcept [[ pre: x != 0 ]] {}
