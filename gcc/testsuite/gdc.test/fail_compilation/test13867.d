/*
TEST_OUTPUT:
---
fail_compilation/test13867.d(12): Error: function test13867.X.blah does not override any function, did you mean to override 'extern (C++) test13867.Base.blah'?
fail_compilation/test13867.d(19): Error: function test13867.Z.blah does not override any function, did you mean to override 'extern (C++) test13867.Base.blah'?
---
*/
extern (C++) class Base {
    void blah() {}
}
class X : Base {
    override void blah();//Error
}
extern (C++) class Y : Base {
    override void blah(){}
}
class Z : Base {
    alias blah = super.blah;
    override void blah(){}//Error
}
class O : Base {
    extern (C++) override void blah(){}
}
extern (C++) class OK : Base {
    alias blah = super.blah;
    override void blah(){}
}

void main() {
    scope b = new Base();
    b.blah();
    scope x = new X();
    x.blah();
    scope y = new Y();
    y.blah();
    scope o = new O();
    o.blah();
    scope z = new Z();
    z.blah();
}
