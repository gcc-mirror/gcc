/// Test storage class inference on delegate parameters

alias FPT = void function (in string, ref string, out string, scope string);
alias DGT = void delegate (in string, ref string, out string, scope string);

void f1 (FPT func)
{
    string ro = "Hello World";
    string ref_ = ro, out_ = ro;
    func(ro, ref_, out_, ro);
}

void f2 (DGT func)
{
    string ro = "Hello World";
    string ref_ = ro, out_ = ro;
    func(ro, ref_, out_, ro);
}

void test ()
{
    f1((in_, ref_, out_, scope_) {
            assert(in_ == "Hello World");
            assert(in_ == scope_);
            assert(in_ == ref_);
            assert(out_ is null);
        });

    f2((in_, ref_, out_, scope_) {
            assert(in_ == "Hello World");
            assert(in_ == scope_);
            assert(in_ == ref_);
            assert(out_ is null);
        });
}

// https://issues.dlang.org/show_bug.cgi?id=11316
void issue11316() {
    void delegate(const int x) F0;
    F0 = (const int x) {}; // OK
    F0 = (x) {};           // OK
    void delegate(in int x) F1;
    F1 = (in int x) {};    // OK
    F1 = (x) {};           // OK
    void delegate(ref int x) F2;
    F2 = (ref int x) {};   // OK
    F2 = (x) {};           // Error
    void delegate(out int x) F3;
    F3 = (out int x) {};   // OK
    F3 = (x) {};           // Error
}
