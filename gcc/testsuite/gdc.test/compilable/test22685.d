// EXTRA_FILES: imports/test22685b.d imports/test22685c.d

module test22685;

import imports.test22685b;

void twoArgs(alias a, alias b)() { }

void main() {
    twoArgs!(a => 1, overloaded);
}
