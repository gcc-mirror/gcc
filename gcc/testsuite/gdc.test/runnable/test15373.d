// https://issues.dlang.org/show_bug.cgi?id=15373

// Using `typeid` on an `extern(C++) class` type is fine as it is evaluated at compile-time

// Using `typeid` on an `extern(C++) class` instance is not ok because `extern(C++) class`
// instances are not rooted in `Object`.  See test/fail_compilation/test15373.d

extern(C++) class C
{ }

void main()
{
    auto Cti = typeid(C);
    assert(Cti.name == "test15373.C");
}
