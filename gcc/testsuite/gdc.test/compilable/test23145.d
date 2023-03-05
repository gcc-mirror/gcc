/* TEST_OUTPUT:
---
compilable/test23145.d(117): Deprecation: `scope` allocation of `c` requires that constructor be annotated with `scope`
compilable/test23145.d(111):        is the location of the constructor
compilable/test23145.d(124): Deprecation: `scope` allocation of `c` requires that constructor be annotated with `scope`
compilable/test23145.d(111):        is the location of the constructor
---
*/

// https://issues.dlang.org/show_bug.cgi?id=23145

#line 100

class D
{
    C c;
}

class C
{
    D d;
    int x=3;
    this(int i) scope @safe @nogc;
    this(D d) @safe @nogc;
}

C foo(D d)@nogc @safe
{
    scope e = new C(1);  // ok
    scope c = new C(d);  // deprecation
    return c.d.c;
}

C bax(D d) @safe
{
    scope e = new C(1);  // ok
    scope c = new C(d);  // deprecation
    return c.d.c;
}
