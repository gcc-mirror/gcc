/*
TEST_OUTPUT:
---
fail_compilation/parseStc5.d(10): Error: constructor cannot be static
fail_compilation/parseStc5.d(11): Error: postblit cannot be `static`
---
*/
class C1
{
    static pure this(int) {}        // `static pure` + `this(int)`
    static pure this(this) {}       // `static pure` + `this(this)`
}

/*
TEST_OUTPUT:
---
fail_compilation/parseStc5.d(27): Error: use `shared static this()` to declare a shared static constructor
fail_compilation/parseStc5.d(28): Error: use `shared static this()` to declare a shared static constructor
fail_compilation/parseStc5.d(30): Error: use `shared static this()` to declare a shared static constructor
fail_compilation/parseStc5.d(32): Error: use `shared static ~this()` to declare a shared static destructor
fail_compilation/parseStc5.d(33): Error: use `shared static ~this()` to declare a shared static destructor
fail_compilation/parseStc5.d(35): Error: use `shared static ~this()` to declare a shared static destructor
---
*/
class C2    // wrong combinations of `shared`, `static`, and `~?this()`
{
    shared pure static this() {}    // `shared pure` + `static this()`
    shared static pure this() {}    // `shared static pure` + `this()`

    static this() shared {}         // `shared pure` + `static this()`

    shared pure static ~this() {}   // `shared pure` + `static ~this()`
    shared static pure ~this() {}   // `shared static pure` + `~this()`

    static ~this() shared {}        // `shared` + `static ~this()`
}

/*
TEST_OUTPUT:
---
fail_compilation/parseStc5.d(47): Error: use `static this()` to declare a static constructor
fail_compilation/parseStc5.d(48): Error: use `static ~this()` to declare a static destructor
---
*/
class C3    // wrong combinations of `static` and `~?this()`
{
    static pure this() {}           // `static pure` + `this()`
    static pure ~this() {}          // `static pure` + `~this()`
}

/*
TEST_OUTPUT:
---
fail_compilation/parseStc5.d(63): Error: redundant attribute `shared`
fail_compilation/parseStc5.d(64): Error: redundant attribute `shared`
fail_compilation/parseStc5.d(66): Error: redundant attribute `static`
fail_compilation/parseStc5.d(68): Error: redundant attribute `static shared`
fail_compilation/parseStc5.d(69): Error: redundant attribute `static shared`
---
*/
class C4    // redundancy of `shared` and/or `static`
{
    shared shared static this() {}                  // `shared` + `shared static this()`
    shared static this() shared {}                  // `shared` + `shared static this()`

    static static this() {}                         // `static` + `shared static this()`

    shared static shared static this() {}           // shared static + `shared static this()`
    shared static shared static this() shared {}    // shared shared static + `shared static this()`
}

/*
TEST_OUTPUT:
---
fail_compilation/parseStc5.d(83): Error: static constructor cannot be `const`
fail_compilation/parseStc5.d(84): Error: static destructor cannot be `const`
fail_compilation/parseStc5.d(85): Error: shared static constructor cannot be `const`
fail_compilation/parseStc5.d(86): Error: shared static destructor cannot be `const`
---
*/
class C5    // wrong MemberFunctionAttributes on `shared? static (con|de)structor`
{
    static this() const {}
    static ~this() const {}
    shared static this() const {}
    shared static ~this() const {}
}
