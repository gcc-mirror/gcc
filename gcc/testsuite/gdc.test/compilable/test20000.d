// https://issues.dlang.org/show_bug.cgi?id=20000

interface A { int a(); }
interface B { int b(); }
class C {}

bool isA(Object x) @safe { return cast(A) x !is null; }
bool isA(B x) @safe { return cast(A) x !is null; }
bool isA(C x) @safe { return cast(A) x !is null; }
