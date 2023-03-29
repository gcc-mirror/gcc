mixin template Constructors(){
    this(){ }
    this()immutable{ }
    this()shared{ }
}

class A {
public:
    static T getInstance(this T)() {
        return new T();
    }
private:
    mixin Constructors;
}

class B : A {
private:
    mixin Constructors;
}

void f(){
    auto a = (new A).getInstance;
    auto b = (new B).getInstance;
    static assert(is(typeof(a) == A));
    static assert(is(typeof(b) == B));

    auto ca = (new immutable A).getInstance;
    auto sb = (new shared B).getInstance;
    static assert(is(typeof(ca) == immutable A));
    static assert(is(typeof(sb) == shared B));
}

// https://issues.dlang.org/show_bug.cgi?id=10488
version(none)
void g(){
    auto a = A.getInstance();
    auto b = B.getInstance();
    static assert(is(typeof(a)==A));
    static assert(is(typeof(b)==B));

    auto ai = (immutable(A)).getInstance();
    auto bs = (shared(B)).getInstance();
    static assert(is(typeof(ai)==immutable(A)));
    static assert(is(typeof(bs)==shared(B)));
}
