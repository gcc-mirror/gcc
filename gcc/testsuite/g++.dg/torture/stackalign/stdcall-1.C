// { dg-do compile { target { { i?86-*-* x86_64-*-* } && ia32 } } }

// This case is to detect an assertion failure in stack branch development.

bool f();
struct S {
    __attribute__ ((stdcall)) ~S();
};
void g() {
    for (;;) {
        S s1, s2, s3;
        if (f())
            continue;
        if (f())
            return;
    }
}
