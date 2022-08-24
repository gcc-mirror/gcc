// https://issues.dlang.org/show_bug.cgi?id=16472
enum e() = 0;

template t(alias v = e!()) {} //Error
alias dummy = t!(e!());

template E(F){
    enum E {
        K = F(1)
    }
}

struct S(F = float, alias e_ = E!double.K) {}
S!float x; // Error: E!double.K is used as a type

alias T = E!double.K;
struct S2(F = float, alias e_ = T) {}
S2!float y; // alias makes it okay...

struct S3(F = float, alias e_ = (E!double.K)) {}
S3!float z; // just putting parens make it okay as well... wat!?

// for coverage

template G(T)
{
    struct G
    {
        alias I = int;
        static int i;
    }
}

struct H(F = float, alias e_ = G!double) {}
H!float a;

struct H1(F = float, alias e_ = G!double.I) {}
H1!float b;

// https://issues.dlang.org/show_bug.cgi?id=21795
// struct H2(F = float, alias e_ = G!double.i) {}
// H2!float c;
