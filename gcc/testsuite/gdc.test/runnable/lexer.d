// REQUIRED_ARGS:


/*********************************************************/

void test6()
{
    string s = q"(foo(xxx))";
    assert(s == "foo(xxx)");

    s = q"[foo[xxx]]";
    assert(s == "foo[xxx]");

    s = q"{foo{xxx}}";
    assert(s == "foo{xxx}");

    s = q"<foo<xxx>>";
    assert(s == "foo<xxx>");

    s = q"[foo(]";
    assert(s == "foo(");

    s = q"/foo]/";
    assert(s == "foo]");


    s = q"HERE
foo
HERE";
    //writefln("'%s'", s);
    assert(s == "foo\n");


    s = q{ foo(xxx) };
    assert(s ==" foo(xxx) ");

    s = q{foo(};
    assert(s == "foo(");

    s = q{{foo}/*}*/};
    assert(s == "{foo}/*}*/");

    s = q{{foo}"}"};
    assert(s == "{foo}\"}\"");
}

/*********************************************************/

void test7()
{
//    auto str = \xDB;
//    assert(str.length == 1);
}

/*********************************************************/
// 4633

template Types(alias v)
{
    alias typeof(v) Types;
}

typeof({return 1;}()) a; // ok
Types!({return 1;}()) x; // ok

void test8()
{
    typeof({return 1;}()) b;
    Types!({return 1;}()) y;
}

/*********************************************************/

// bug 6584
version(9223372036854775807){}
debug(9223372036854775807){}

/*********************************************************/

enum e13102=184467440737095516153.6L;

/*********************************************************/

int main()
{
    test6();
    test7();
    test8();

    return 0;
}
