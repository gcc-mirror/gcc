// EXTRA_FILES: imports/bar10378.d
int writeln() { return 3; }

struct S {
    import imports.bar10378;
    void abc() { assert(writeln() == 3); }
}


void main() {
    S s;
    s.abc();
}
