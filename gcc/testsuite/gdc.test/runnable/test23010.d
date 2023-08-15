// https://issues.dlang.org/show_bug.cgi?id=23010

alias AliasSeq(T...) = T;

mixin template faz() {
    alias T = AliasSeq!(int);
    T bar = 12345;

    void write1() {
        assert(bar[0] == 12345);
    }

    AliasSeq!(string, float) foo = AliasSeq!("qwerty", 1.25f);

    void write2() {
        assert(foo == AliasSeq!("qwerty", 1.25f));
        foo = AliasSeq!("asdfg", 2.5f); // this even crashed before
        assert(foo == AliasSeq!("asdfg", 2.5f));
    }
}

void main() {
    mixin faz!();
    write1;
    write2;
    fun;
}

// Testing static symbol generation ('toobj.d' changes)

static AliasSeq!(int, string) tup;

void fun()
{
    auto v = tup;

    struct S(T...) {
        static T b;
    }

    alias T = S!(int, float);
    auto p = T.b;
}
