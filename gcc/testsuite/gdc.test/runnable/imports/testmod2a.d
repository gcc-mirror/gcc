module imports.testmod2a;

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=1904

// testmod.d
private void bar(alias a)() {}

void foo(alias a)() {
    .bar!(a)();
}
