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

    // https://issues.dlang.org/show_bug.cgi?id=19623
    s = q"übel
foo
übel";
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
// https://issues.dlang.org/show_bug.cgi?id=4633

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

enum e13102=184467440737095516153.6L;

/*********************************************************/

static assert("\&acE;" == "\U0000223E\U00000333"); // ="\xe2\x88\xbe\xcc\xb3"

/*********************************************************/

int main()
{
    test6();
    test7();
    test8();

    return 0;
}
