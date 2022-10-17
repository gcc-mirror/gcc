/*
TEST_OUTPUT:
---
fail_compilation/diag1730.d(51): Error: mutable method `diag1730.S.func` is not callable using a `inout` object
fail_compilation/diag1730.d(43):        Consider adding `const` or `inout` here
fail_compilation/diag1730.d(53): Error: `immutable` method `diag1730.S.iFunc` is not callable using a `inout` object
fail_compilation/diag1730.d(54): Error: `shared` mutable method `diag1730.S.sFunc` is not callable using a non-shared `inout` object
fail_compilation/diag1730.d(46):        Consider adding `const` or `inout` here
fail_compilation/diag1730.d(55): Error: `shared` `const` method `diag1730.S.scFunc` is not callable using a non-shared `inout` object
fail_compilation/diag1730.d(70): Error: `immutable` method `diag1730.S.iFunc` is not callable using a mutable object
fail_compilation/diag1730.d(71): Error: `shared` method `diag1730.S.sFunc` is not callable using a non-shared object
fail_compilation/diag1730.d(72): Error: `shared` `const` method `diag1730.S.scFunc` is not callable using a non-shared mutable object
fail_compilation/diag1730.d(75): Error: mutable method `diag1730.S.func` is not callable using a `const` object
fail_compilation/diag1730.d(43):        Consider adding `const` or `inout` here
fail_compilation/diag1730.d(77): Error: `immutable` method `diag1730.S.iFunc` is not callable using a `const` object
fail_compilation/diag1730.d(78): Error: `shared` mutable method `diag1730.S.sFunc` is not callable using a non-shared `const` object
fail_compilation/diag1730.d(46):        Consider adding `const` or `inout` here
fail_compilation/diag1730.d(79): Error: `shared` `const` method `diag1730.S.scFunc` is not callable using a non-shared `const` object
fail_compilation/diag1730.d(82): Error: mutable method `diag1730.S.func` is not callable using a `immutable` object
fail_compilation/diag1730.d(43):        Consider adding `const` or `inout` here
fail_compilation/diag1730.d(85): Error: `shared` mutable method `diag1730.S.sFunc` is not callable using a `immutable` object
fail_compilation/diag1730.d(46):        Consider adding `const` or `inout` here
fail_compilation/diag1730.d(89): Error: non-shared method `diag1730.S.func` is not callable using a `shared` object
fail_compilation/diag1730.d(43):        Consider adding `shared` here
fail_compilation/diag1730.d(90): Error: non-shared `const` method `diag1730.S.cFunc` is not callable using a `shared` mutable object
fail_compilation/diag1730.d(44):        Consider adding `shared` here
fail_compilation/diag1730.d(91): Error: `immutable` method `diag1730.S.iFunc` is not callable using a `shared` mutable object
fail_compilation/diag1730.d(94): Error: non-shared `inout` method `diag1730.S.wFunc` is not callable using a `shared` mutable object
fail_compilation/diag1730.d(48):        Consider adding `shared` here
fail_compilation/diag1730.d(96): Error: non-shared mutable method `diag1730.S.func` is not callable using a `shared` `const` object
fail_compilation/diag1730.d(43):        Consider adding `shared` here
fail_compilation/diag1730.d(97): Error: non-shared `const` method `diag1730.S.cFunc` is not callable using a `shared` `const` object
fail_compilation/diag1730.d(44):        Consider adding `shared` here
fail_compilation/diag1730.d(98): Error: `immutable` method `diag1730.S.iFunc` is not callable using a `shared` `const` object
fail_compilation/diag1730.d(99): Error: `shared` mutable method `diag1730.S.sFunc` is not callable using a `shared` `const` object
fail_compilation/diag1730.d(46):        Consider adding `const` or `inout` here
fail_compilation/diag1730.d(101): Error: non-shared `inout` method `diag1730.S.wFunc` is not callable using a `shared` `const` object
fail_compilation/diag1730.d(48):        Consider adding `shared` here
---
*/
struct S
{
    void func() { }
    void cFunc() const { }
    void iFunc() immutable { }
    void sFunc() shared { }
    void scFunc() shared const { }
    void wFunc() inout { }
    static void test(inout(S) s)
    {
        s.func();   // ng
        s.cFunc();
        s.iFunc();  // ng
        s.sFunc();  // ng
        s.scFunc(); // ng
        s.wFunc();
    }
}

void main()
{
    S obj;
    const(S) cObj;
    immutable(S) iObj;
    shared(S) sObj;
    shared(const(S)) scObj;

    obj.func();
    obj.cFunc();
    obj.iFunc();   // ng
    obj.sFunc();   // ng
    obj.scFunc();  // ng
    obj.wFunc();

    cObj.func();   // ng
    cObj.cFunc();
    cObj.iFunc();  // ng
    cObj.sFunc();  // ng
    cObj.scFunc(); // ng
    cObj.wFunc();

    iObj.func();   // ng
    iObj.cFunc();
    iObj.iFunc();
    iObj.sFunc();  // ng
    iObj.scFunc();
    iObj.wFunc();

    sObj.func();   // ng
    sObj.cFunc();  // ng
    sObj.iFunc();  // ng
    sObj.sFunc();
    sObj.scFunc();
    sObj.wFunc();  // ng

    scObj.func();  // ng
    scObj.cFunc(); // ng
    scObj.iFunc(); // ng
    scObj.sFunc(); // ng
    scObj.scFunc();
    scObj.wFunc(); // ng
}
