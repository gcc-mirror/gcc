// { dg-additional-options {-fmodules-ts} }
export module test;
// { dg-module-cmi test }
import "pr99170-1_a.H";
export class A {
  STD::string str{"ayyy"};
};
