/* REQUIRED_ARGS: -wi
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

C foo(D d) @nogc @safe
{
    scope e = new C(1);  // ok
    scope c = new C(d);  // obsolete
    return c.d.c;
}

C bax(D d) @safe
{
    scope e = new C(1);  // ok
    scope c = new C(d);  // obsolete
    return c.d.c;
}

void inferred(D d)
{
    scope c = new C(d);  // ok
}
