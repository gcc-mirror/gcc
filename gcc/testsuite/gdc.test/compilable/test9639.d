class A { this(A) {} }
class B {}
class C {}

// two sibling nested functions in main
typeof(null) foo(alias fn)(A a) { fn(a); return foo!fn(B.init); }
typeof(null) foo(alias fn)(B b) {        return foo!fn(A.init); }

// three sibling nested functions in main
typeof(null) bar(alias fn)(A a) { fn(a); return bar!fn(B.init); }
typeof(null) bar(alias fn)(B b) {        return bar!fn(C.init); }
typeof(null) bar(alias fn)(C c) {        return bar!fn(A.init); }

void main()
{
    A a;
    foo!((stuff){ new A(a); })(a);
    bar!((stuff){ new A(a); })(a);
}
