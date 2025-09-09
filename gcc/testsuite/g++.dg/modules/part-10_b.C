// { dg-additional-options "-fmodules" }
// { dg-module-cmi foo:part }

module foo:part;
import foo;

void part() {
  foo();
}
