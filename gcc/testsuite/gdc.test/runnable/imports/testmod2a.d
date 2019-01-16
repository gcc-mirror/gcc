module imports.testmod2a;

/**********************************/
// bug 1904

// testmod.d
private void bar(alias a)() {}

void foo(alias a)() {
    .bar!(a)();
}
