// { dg-additional-options "-fmodules" }

module foo;
import :trans;

void impl() {
  foo();
  part();
  trans();
}
