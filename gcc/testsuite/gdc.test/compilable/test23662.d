// https://issues.dlang.org/show_bug.cgi?id=23662
// EXTRA_FILES: imports/imp23662.c
import imports.imp23662;

void main(string[] args) {
    auto r = func(A);
    assert(r == A);
}
