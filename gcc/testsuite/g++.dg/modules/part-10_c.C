// { dg-additional-options "-fmodules" }
// { dg-module-cmi foo:trans }

module foo:trans;
import :part;

void trans() {
  foo();
  part();
}
