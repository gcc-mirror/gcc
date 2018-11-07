module test;

mixin template A() {
import test;
}

struct B {
mixin A!();
}

enum C = B.nonexistent;

